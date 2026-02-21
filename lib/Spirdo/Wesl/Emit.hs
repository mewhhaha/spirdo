{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | SPIR-V emission backend.
module Spirdo.Wesl.Emit where

import Control.Monad (foldM, unless, zipWithM_, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import Data.Foldable (foldrM)
import Data.Int (Int32)
import Data.List (find, intercalate, mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word16, Word32)
import GHC.Float (castFloatToWord32, castWord32ToFloat)
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH as TH
import Spirdo.Wesl.Emit.Encoding (encodeString, spirvToBytes)
import Spirdo.Wesl.Syntax
import Spirdo.Wesl.Typecheck
import Spirdo.Wesl.Types
import Spirdo.Wesl.Util
import Text.Read (readMaybe)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- Interface and layout

buildInterface :: CompileOptions -> ModuleAst -> Either CompileError ShaderInterface
buildInterface opts modAst = do
  let structEnv = [(s.sdName, s) | s <- modAst.modStructs]
  structLayouts <- resolveStructLayouts structEnv
  bindings0 <- mapM (layoutBinding structLayouts) (modAst.modBindings)
  let bindings =
        case opts.samplerBindingMode of
          SamplerCombined -> filter (not . isSamplerKind . (.biKind)) bindings0
          SamplerSeparate -> bindings0
  checkBindingInvariants bindings
  overrides <- buildOverrideInfo opts.overrideSpecMode structLayouts (modAst.modOverrides)
  entry <- selectEntryPoint opts modAst.modEntries
  stageInfo <- buildStageIO structLayouts structEnv entry
  pure (ShaderInterface bindings overrides stageInfo Nothing opts.samplerBindingMode)

selectEntryPoint :: CompileOptions -> [EntryPoint] -> Either CompileError (Maybe EntryPoint)
selectEntryPoint opts entries =
  case entries of
    [] -> Right Nothing
    [entry] -> Right (Just entry)
    _ ->
      case opts.entryPointName of
        Nothing ->
          Left (CompileError "multiple entry points found; select one with withEntryPoint" Nothing Nothing)
        Just name ->
          case find (\e -> e.epName == T.pack name) entries of
            Nothing ->
              Left (CompileError ("entry point not found: " <> name) Nothing Nothing)
            Just entry -> Right (Just entry)

checkBindingInvariants :: [BindingInfo] -> Either CompileError ()
checkBindingInvariants bindings = do
  let dupNames = duplicates (.biName) bindings
  unless (null dupNames) $
    Left (CompileError ("duplicate binding names: " <> intercalate ", " dupNames) Nothing Nothing)
  let dupLocs = duplicates (\b -> (b.biGroup, b.biBinding)) bindings
  unless (null dupLocs) $
    Left (CompileError ("duplicate binding locations: " <> intercalate ", " (map showLoc dupLocs)) Nothing Nothing)
  where
    duplicates :: Ord k => (BindingInfo -> k) -> [BindingInfo] -> [k]
    duplicates key xs =
      [k | (k, count) <- Map.toList (Map.fromListWith (+) [(key x, 1 :: Int) | x <- xs]), count > 1]

    showLoc (g, b) = "group " <> show g <> " binding " <> show b

buildStageIO :: StructLayoutCache -> [(Text, StructDecl)] -> Maybe EntryPoint -> Either CompileError (Maybe StageIO)
buildStageIO _ _ Nothing = Right Nothing
buildStageIO layoutCache structEnv (Just entry) = do
  inputs <- collectStageInputs layoutCache structEnv entry
  outputs <- collectStageOutputs layoutCache structEnv entry
  let stage = toShaderStage (entry.epStage)
  wgSize <-
    case entry.epWorkgroupSize of
      Nothing -> Right Nothing
      Just (WorkgroupSizeValue v) -> Right (Just v)
      Just (WorkgroupSizeExpr _) ->
        Left (CompileError "workgroup_size expressions must be resolved before interface build" Nothing Nothing)
  pure (Just (StageIO stage wgSize inputs outputs))

toShaderStage :: Stage -> ShaderStage
toShaderStage st =
  case st of
    StageCompute -> ShaderStageCompute
    StageFragment -> ShaderStageFragment
    StageVertex -> ShaderStageVertex

collectStageInputs :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> Either CompileError [IOParam]
collectStageInputs layoutCache structEnv entry =
  fmap concat (mapM (paramInputs layoutCache structEnv) (entry.epParams))

paramInputs :: StructLayoutCache -> [(Text, StructDecl)] -> Param -> Either CompileError [IOParam]
paramInputs layoutCache structEnv param =
  case param.paramType of
    TyStructRef structName -> do
      structDecl <- lookupStruct structEnv structName
      mapM (fieldInput param.paramName structName) structDecl.sdFields
    ty -> do
      layout <- resolveTypeLayoutWithCache layoutCache ty
      fmap pure (mkIOParam (textToString param.paramName) param.paramAttrs layout)
  where
    fieldInput parentName structName field = do
      layout <- resolveTypeLayoutWithCache layoutCache field.fdType
      let label = qualifyField parentName structName field.fdName
      mkIOParam label field.fdAttrs layout

collectStageOutputs :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> Either CompileError [IOParam]
collectStageOutputs layoutCache structEnv entry =
  case entry.epStage of
    StageCompute -> Right []
    _ ->
      case entry.epReturnType of
        Nothing -> Right []
        Just ty ->
          case ty of
            TyStructRef structName -> do
              structDecl <- lookupStruct structEnv structName
              mapM (fieldOutput structName) (structDecl.sdFields)
            _ -> do
              layout <- resolveTypeLayoutWithCache layoutCache ty
              let builtin = entry.epReturnBuiltin
              let loc =
                    if isNothing builtin && isNothing entry.epReturnLocation && entry.epStage == StageFragment
                      then Just 0
                      else entry.epReturnLocation
              fmap pure (mkReturnIOParam layout loc builtin)
  where
    fieldOutput structName field = do
      layout <- resolveTypeLayoutWithCache layoutCache field.fdType
      let label = qualifyField structName structName field.fdName
      mkIOParam label field.fdAttrs layout

lookupStruct :: [(Text, StructDecl)] -> Text -> Either CompileError StructDecl
lookupStruct structEnv structName =
  case lookup structName structEnv of
    Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
    Just decl -> Right decl

qualifyField :: Text -> Text -> Text -> String
qualifyField parentName structName fieldName =
  let parent =
        if parentName == structName
          then textToString structName
          else textToString parentName
  in parent <> "." <> textToString fieldName

mkIOParam :: String -> [Attr] -> TypeLayout -> Either CompileError IOParam
mkIOParam label attrs layout = do
  let loc = attrLocation attrs
  let builtin = attrBuiltin attrs
  when (isJust loc && isJust builtin) $
    Left (CompileError "input/output cannot use both @location and @builtin" Nothing Nothing)
  when (isNothing loc && isNothing builtin) $
    Left (CompileError "input/output is missing @location or @builtin" Nothing Nothing)
  pure
    IOParam
      { ioName = label
      , ioLocation = loc
      , ioBuiltin = textToString <$> builtin
      , ioType = layout
      }

mkReturnIOParam :: TypeLayout -> Maybe Word32 -> Maybe Text -> Either CompileError IOParam
mkReturnIOParam layout loc builtin = do
  let builtinStr = textToString <$> builtin
  when (isJust loc && isJust builtin) $
    Left (CompileError "return value cannot use both @location and @builtin" Nothing Nothing)
  when (isNothing loc && isNothing builtin) $
    Left (CompileError "return value is missing @location or @builtin" Nothing Nothing)
  pure
    IOParam
      { ioName = "return"
      , ioLocation = loc
      , ioBuiltin = builtinStr
      , ioType = layout
      }

buildOverrideInfo :: OverrideSpecMode -> StructLayoutCache -> [OverrideDecl] -> Either CompileError [OverrideInfo]
buildOverrideInfo specMode layoutCache decls = do
  specIds <- assignOverrideSpecIds decls
  let depsMap = overrideDependencies decls
  _ <- topoSortOverrides (map (.odName) decls) depsMap
  mapM (layoutOverride specMode specIds depsMap layoutCache) decls

layoutOverride :: OverrideSpecMode -> Map.Map Text Word32 -> Map.Map Text (Set.Set Text) -> StructLayoutCache -> OverrideDecl -> Either CompileError OverrideInfo
layoutOverride specMode specIds depsMap layoutCache decl = do
  ty <- maybe (Left (CompileError "override type could not be inferred" Nothing Nothing)) Right decl.odType
  layout <- resolveTypeLayoutWithCache layoutCache ty
  let deps = Map.findWithDefault Set.empty decl.odName depsMap
  specId <-
    case specMode of
      SpecParity ->
        case Map.lookup decl.odName specIds of
          Just sid -> Right (Just sid)
          Nothing -> Left (CompileError "missing specialization id for override" Nothing Nothing)
      SpecStrict ->
        if Set.null deps
          then case Map.lookup decl.odName specIds of
            Just sid -> Right (Just sid)
            Nothing -> Left (CompileError "missing specialization id for override" Nothing Nothing)
          else Right Nothing
  pure (OverrideInfo (textToString decl.odName) decl.odId specId layout)

assignOverrideSpecIds :: [OverrideDecl] -> Either CompileError (Map.Map Text Word32)
assignOverrideSpecIds overrides = do
  let explicit = [(d.odName, i) | d <- overrides, Just i <- [d.odId]]
  let ids = map snd explicit
  let (dups, _) = foldl' collect ([], Set.empty) ids
  unless (null dups) $
    Left (CompileError ("duplicate override ids: " <> intercalate ", " (map show dups)) Nothing Nothing)
  let used0 = Set.fromList ids
  let acc0 = Map.fromList explicit
  (acc, _, _) <- foldM assign (acc0, used0, 0) overrides
  Right acc
  where
    collect (acc, seen) n =
      if Set.member n seen
        then (n : acc, seen)
        else (acc, Set.insert n seen)

    assign (acc, used, next) decl =
      case decl.odId of
        Just _ -> Right (acc, used, next)
        Nothing -> do
          (specId, used', next') <- nextSpecId used next
          Right (Map.insert decl.odName specId acc, used', next')

    nextSpecId used next =
      if Set.member next used
        then
          if next == maxBound
            then Left (CompileError "ran out of override specialization ids" Nothing Nothing)
          else nextSpecId used (next + 1)
        else Right (next, Set.insert next used, next + 1)

overrideDependencies :: [OverrideDecl] -> Map.Map Text (Set.Set Text)
overrideDependencies decls =
  let names = Set.fromList (map (.odName) decls)
  in Map.fromList [(d.odName, maybe Set.empty (collectOverrideRefs names) d.odExpr) | d <- decls]

collectOverrideRefs :: Set.Set Text -> Expr -> Set.Set Text
collectOverrideRefs names expr =
  case expr of
    EVar _ n | Set.member n names -> Set.singleton n
    EUnary _ _ e -> collectOverrideRefs names e
    EBinary _ _ a b -> collectOverrideRefs names a <> collectOverrideRefs names b
    ECall _ _ args -> foldMap (collectOverrideRefs names) args
    EBitcast _ _ e -> collectOverrideRefs names e
    EField _ e _ -> collectOverrideRefs names e
    EIndex _ a b -> collectOverrideRefs names a <> collectOverrideRefs names b
    _ -> Set.empty

topoSortOverrides :: [Text] -> Map.Map Text (Set.Set Text) -> Either CompileError [Text]
topoSortOverrides order depsMap = go Set.empty Set.empty [] order
  where
    go _ _ acc [] = Right (reverse acc)
    go temp perm acc (n:ns)
      | Set.member n perm = go temp perm acc ns
      | Set.member n temp =
          Left (CompileError ("override dependency cycle involving " <> textToString n) Nothing Nothing)
      | otherwise = do
          (perm', acc') <- visit temp (perm, acc) n
          go temp perm' acc' ns

    visit temp (perm, acc) n
      | Set.member n perm = Right (perm, acc)
      | Set.member n temp =
          Left (CompileError ("override dependency cycle involving " <> textToString n) Nothing Nothing)
      | otherwise = do
          let temp' = Set.insert n temp
          let deps = Set.toList (Map.findWithDefault Set.empty n depsMap)
          (perm', acc') <- foldM (visit temp') (perm, acc) deps
          let perm'' = Set.insert n perm'
          Right (perm'', n : acc')

layoutBinding :: StructLayoutCache -> BindingDecl -> Either CompileError BindingInfo
layoutBinding layoutCache decl = do
  case decl.bdKind of
    BUniform -> ensureStructBinding
    BStorageRead -> ensureStructBinding
    BStorageReadWrite -> ensureStructBinding
    BSampler ->
      case decl.bdType of
        TySampler -> pure ()
        _ -> Left (CompileError "sampler bindings must use type sampler" Nothing Nothing)
    BSamplerComparison ->
      case decl.bdType of
        TySamplerComparison -> pure ()
        _ -> Left (CompileError "sampler_comparison bindings must use type sampler_comparison" Nothing Nothing)
    BTexture1D ->
      case decl.bdType of
        TyTexture1D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_1d<scalar>" Nothing Nothing)
    BTexture1DArray ->
      case decl.bdType of
        TyTexture1DArray _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_1d_array<scalar>" Nothing Nothing)
    BTexture2D ->
      case decl.bdType of
        TyTexture2D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_2d<scalar>" Nothing Nothing)
    BTexture2DArray ->
      case decl.bdType of
        TyTexture2DArray _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_2d_array<scalar>" Nothing Nothing)
    BTexture3D ->
      case decl.bdType of
        TyTexture3D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_3d<scalar>" Nothing Nothing)
    BTextureCube ->
      case decl.bdType of
        TyTextureCube _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_cube<scalar>" Nothing Nothing)
    BTextureCubeArray ->
      case decl.bdType of
        TyTextureCubeArray _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_cube_array<scalar>" Nothing Nothing)
    BTextureMultisampled2D ->
      case decl.bdType of
        TyTextureMultisampled2D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_multisampled_2d<scalar>" Nothing Nothing)
    BTextureDepth2D ->
      case decl.bdType of
        TyTextureDepth2D -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_2d" Nothing Nothing)
    BTextureDepth2DArray ->
      case decl.bdType of
        TyTextureDepth2DArray -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_2d_array" Nothing Nothing)
    BTextureDepthCube ->
      case decl.bdType of
        TyTextureDepthCube -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_cube" Nothing Nothing)
    BTextureDepthCubeArray ->
      case decl.bdType of
        TyTextureDepthCubeArray -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_cube_array" Nothing Nothing)
    BTextureDepthMultisampled2D ->
      case decl.bdType of
        TyTextureDepthMultisampled2D -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_depth_multisampled_2d" Nothing Nothing)
    BStorageTexture1D ->
      case decl.bdType of
        TyStorageTexture1D _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_1d<format, access>" Nothing Nothing)
    BStorageTexture2D ->
      case decl.bdType of
        TyStorageTexture2D _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_2d<format, access>" Nothing Nothing)
    BStorageTexture2DArray ->
      case decl.bdType of
        TyStorageTexture2DArray _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_2d_array<format, access>" Nothing Nothing)
    BStorageTexture3D ->
      case decl.bdType of
        TyStorageTexture3D _ _ -> pure ()
        _ -> Left (CompileError "storage texture bindings must use type texture_storage_3d<format, access>" Nothing Nothing)
  tyLayout <- resolveTypeLayoutWithCache layoutCache (decl.bdType)
  when (containsPointer tyLayout) $
    Left (CompileError "bindings cannot contain pointer types" Nothing Nothing)
  case decl.bdKind of
    BUniform -> do
      when (containsAtomic tyLayout) $
        Left (CompileError "uniform bindings cannot contain atomic types" Nothing Nothing)
      when (containsRuntimeArray tyLayout) $
        Left (CompileError "uniform bindings cannot contain runtime arrays" Nothing Nothing)
    _ -> pure ()
  pure (BindingInfo (textToString decl.bdName) decl.bdKind decl.bdGroup decl.bdBinding tyLayout)
  where
    ensureStructBinding =
      case decl.bdType of
        TyStructRef _ -> pure ()
        _ -> Left (CompileError "bindings must use a struct type (wrap arrays in a struct)" Nothing Nothing)

collectSamplerLayouts :: StructLayoutCache -> [BindingDecl] -> Either CompileError (Map.Map Text TypeLayout)
collectSamplerLayouts layoutCache decls = do
  infos <- mapM (layoutBinding layoutCache) decls
  let samplerInfos = filter (isSamplerKind . (.biKind)) infos
  pure (Map.fromList [(T.pack info.biName, info.biType) | info <- samplerInfos])

type StructLayoutCache = Map.Map Text TypeLayout

resolveStructLayouts :: [(Text, StructDecl)] -> Either CompileError StructLayoutCache
resolveStructLayouts env = foldM step Map.empty env
  where
    step cache (name, _) = do
      (_, cache') <- resolveTypeLayoutCached env cache Set.empty (TyStructRef name)
      Right cache'

resolveTypeLayoutWithCache :: StructLayoutCache -> Type -> Either CompileError TypeLayout
resolveTypeLayoutWithCache cache ty =
  case ty of
    TyScalar s ->
      let (a, sz) = scalarLayout s
      in Right (TLScalar s a sz)
    TyVector n s ->
      let (a, sz) = vectorLayout s n
      in Right (TLVector n s a sz)
    TyMatrix cols rows s ->
      let (a, sz) = vectorLayout s rows
          stride = roundUp sz a
          total = stride * fromIntegral cols
      in Right (TLMatrix cols rows s a total stride)
    TyArray elemTy mlen -> do
      elemLayout <- resolveTypeLayoutWithCache cache elemTy
      let elemAlign = layoutAlign elemLayout
      let elemSize = layoutSize elemLayout
      let stride = roundUp elemSize elemAlign
      let (lenOpt, total) = case mlen of
            ArrayLenRuntime -> (Nothing, stride)
            ArrayLenFixed n -> (Just n, stride * fromIntegral n)
            ArrayLenExpr _ -> (Nothing, stride)
      when (case mlen of ArrayLenExpr _ -> True; _ -> False) $
        Left (CompileError "array length expressions must be resolved before layout" Nothing Nothing)
      Right (TLArray lenOpt stride elemLayout elemAlign total)
    TySampler -> Right TLSampler
    TySamplerComparison -> Right TLSamplerComparison
    TyTexture1D s -> Right (TLTexture1D s)
    TyTexture1DArray s -> Right (TLTexture1DArray s)
    TyTexture2D s -> Right (TLTexture2D s)
    TyTexture2DArray s -> Right (TLTexture2DArray s)
    TyTexture3D s -> Right (TLTexture3D s)
    TyTextureCube s -> Right (TLTextureCube s)
    TyTextureCubeArray s -> Right (TLTextureCubeArray s)
    TyTextureMultisampled2D s -> Right (TLTextureMultisampled2D s)
    TyTextureDepth2D -> Right TLTextureDepth2D
    TyTextureDepth2DArray -> Right TLTextureDepth2DArray
    TyTextureDepthCube -> Right TLTextureDepthCube
    TyTextureDepthCubeArray -> Right TLTextureDepthCubeArray
    TyTextureDepthMultisampled2D -> Right TLTextureDepthMultisampled2D
    TyStorageTexture1D fmt access -> Right (TLStorageTexture1D fmt access)
    TyStorageTexture2D fmt access -> Right (TLStorageTexture2D fmt access)
    TyStorageTexture2DArray fmt access -> Right (TLStorageTexture2DArray fmt access)
    TyStorageTexture3D fmt access -> Right (TLStorageTexture3D fmt access)
    TyAtomic s -> Right (TLAtomic s)
    TyPtr addr access elemTy -> do
      elemLayout <- resolveTypeLayoutWithCache cache elemTy
      when (containsResource elemLayout) $
        Left (CompileError "pointer element types cannot be resources" Nothing Nothing)
      storageClass <- case addr of
        "function" -> Right storageClassFunction
        "private" -> Right storageClassPrivate
        "workgroup" -> Right storageClassWorkgroup
        "uniform" -> Right storageClassUniform
        "storage" -> Right storageClassStorageBuffer
        _ -> Left (CompileError ("unsupported pointer address space: " <> textToString addr) Nothing Nothing)
      access' <- case addr of
        "storage" ->
          case access of
            Nothing -> Right (Just StorageRead)
            Just a -> Right (Just a)
        "uniform" ->
          case access of
            Nothing -> Right (Just StorageRead)
            Just StorageRead -> Right (Just StorageRead)
            _ -> Left (CompileError "uniform pointers must be read-only" Nothing Nothing)
        "function" ->
          if isNothing access then Right Nothing else Left (CompileError "function pointers cannot specify access" Nothing Nothing)
        "private" ->
          if isNothing access then Right Nothing else Left (CompileError "private pointers cannot specify access" Nothing Nothing)
        "workgroup" ->
          if isNothing access then Right Nothing else Left (CompileError "workgroup pointers cannot specify access" Nothing Nothing)
        _ -> Right access
      Right (TLPointer storageClass access' elemLayout)
    TyStructRef name ->
      case Map.lookup name cache of
        Nothing -> Left (CompileError ("unknown struct: " <> textToString name) Nothing Nothing)
        Just layout -> Right layout

resolveTypeLayoutCached :: [(Text, StructDecl)] -> StructLayoutCache -> Set.Set Text -> Type -> Either CompileError (TypeLayout, StructLayoutCache)
resolveTypeLayoutCached env cache visiting ty =
  case ty of
    TyScalar s ->
      let (a, sz) = scalarLayout s
      in Right (TLScalar s a sz, cache)
    TyVector n s ->
      let (a, sz) = vectorLayout s n
      in Right (TLVector n s a sz, cache)
    TyMatrix cols rows s ->
      let (a, sz) = vectorLayout s rows
          stride = roundUp sz a
          total = stride * fromIntegral cols
      in Right (TLMatrix cols rows s a total stride, cache)
    TyArray elemTy mlen -> do
      (elemLayout, cache1) <- resolveTypeLayoutCached env cache visiting elemTy
      let elemAlign = layoutAlign elemLayout
      let elemSize = layoutSize elemLayout
      let stride = roundUp elemSize elemAlign
      let (lenOpt, total) = case mlen of
            ArrayLenRuntime -> (Nothing, stride)
            ArrayLenFixed n -> (Just n, stride * fromIntegral n)
            ArrayLenExpr _ -> (Nothing, stride)
      when (case mlen of ArrayLenExpr _ -> True; _ -> False) $
        Left (CompileError "array length expressions must be resolved before layout" Nothing Nothing)
      Right (TLArray lenOpt stride elemLayout elemAlign total, cache1)
    TySampler -> Right (TLSampler, cache)
    TySamplerComparison -> Right (TLSamplerComparison, cache)
    TyTexture1D s -> Right (TLTexture1D s, cache)
    TyTexture1DArray s -> Right (TLTexture1DArray s, cache)
    TyTexture2D s -> Right (TLTexture2D s, cache)
    TyTexture2DArray s -> Right (TLTexture2DArray s, cache)
    TyTexture3D s -> Right (TLTexture3D s, cache)
    TyTextureCube s -> Right (TLTextureCube s, cache)
    TyTextureCubeArray s -> Right (TLTextureCubeArray s, cache)
    TyTextureMultisampled2D s -> Right (TLTextureMultisampled2D s, cache)
    TyTextureDepth2D -> Right (TLTextureDepth2D, cache)
    TyTextureDepth2DArray -> Right (TLTextureDepth2DArray, cache)
    TyTextureDepthCube -> Right (TLTextureDepthCube, cache)
    TyTextureDepthCubeArray -> Right (TLTextureDepthCubeArray, cache)
    TyTextureDepthMultisampled2D -> Right (TLTextureDepthMultisampled2D, cache)
    TyStorageTexture1D fmt access -> Right (TLStorageTexture1D fmt access, cache)
    TyStorageTexture2D fmt access -> Right (TLStorageTexture2D fmt access, cache)
    TyStorageTexture2DArray fmt access -> Right (TLStorageTexture2DArray fmt access, cache)
    TyStorageTexture3D fmt access -> Right (TLStorageTexture3D fmt access, cache)
    TyAtomic s -> Right (TLAtomic s, cache)
    TyPtr addr access elemTy -> do
      (elemLayout, cache1) <- resolveTypeLayoutCached env cache visiting elemTy
      when (containsResource elemLayout) $
        Left (CompileError "pointer element types cannot be resources" Nothing Nothing)
      storageClass <- case addr of
        "function" -> Right storageClassFunction
        "private" -> Right storageClassPrivate
        "workgroup" -> Right storageClassWorkgroup
        "uniform" -> Right storageClassUniform
        "storage" -> Right storageClassStorageBuffer
        _ -> Left (CompileError ("unsupported pointer address space: " <> textToString addr) Nothing Nothing)
      access' <- case addr of
        "storage" ->
          case access of
            Nothing -> Right (Just StorageRead)
            Just a -> Right (Just a)
        "uniform" ->
          case access of
            Nothing -> Right (Just StorageRead)
            Just StorageRead -> Right (Just StorageRead)
            _ -> Left (CompileError "uniform pointers must be read-only" Nothing Nothing)
        "function" ->
          if isNothing access then Right Nothing else Left (CompileError "function pointers cannot specify access" Nothing Nothing)
        "private" ->
          if isNothing access then Right Nothing else Left (CompileError "private pointers cannot specify access" Nothing Nothing)
        "workgroup" ->
          if isNothing access then Right Nothing else Left (CompileError "workgroup pointers cannot specify access" Nothing Nothing)
        _ -> Right access
      Right (TLPointer storageClass access' elemLayout, cache1)
    TyStructRef name ->
      case Map.lookup name cache of
        Just layout -> Right (layout, cache)
        Nothing ->
          if Set.member name visiting
            then Left (CompileError ("recursive struct: " <> textToString name) Nothing Nothing)
            else case lookup name env of
              Nothing -> Left (CompileError ("unknown struct: " <> textToString name) Nothing Nothing)
              Just decl -> do
                (fields, cache1) <- resolveFieldsCached env cache (Set.insert name visiting) (decl.sdFields)
                let align = maximum (1 : map (.flAlign) fields)
                let size = structSize fields align
                let layout = TLStruct (textToString name) fields align size
                Right (layout, Map.insert name layout cache1)

resolveFieldsCached :: [(Text, StructDecl)] -> StructLayoutCache -> Set.Set Text -> [FieldDecl] -> Either CompileError ([FieldLayout], StructLayoutCache)
resolveFieldsCached env cache visiting fields =
  let go _ acc stCache [] = Right (reverse acc, stCache)
      go offset acc stCache (FieldDecl name fty attrs : rest) = do
        (fLayout, cache1) <- resolveTypeLayoutCached env stCache visiting fty
        if containsResource fLayout
          then Left (CompileError "resource types are not allowed in struct fields" Nothing Nothing)
          else if containsPointer fLayout
            then Left (CompileError "pointer types are not allowed in struct fields" Nothing Nothing)
            else do
              alignAttr <- parseFieldAlign attrs
              sizeAttr <- parseFieldSize attrs
              let baseAlign = layoutAlign fLayout
              let baseSize = layoutSize fLayout
              let requestedAlign = fromMaybe baseAlign alignAttr
              when (requestedAlign < baseAlign) $
                Left (CompileError "@align must be at least the natural alignment" Nothing Nothing)
              let fieldAlign = max baseAlign requestedAlign
              fieldSize <- case sizeAttr of
                Nothing -> Right baseSize
                Just sz ->
                  if sz < baseSize
                    then Left (CompileError "field @size must be at least the natural size" Nothing Nothing)
                    else if sz `mod` fieldAlign /= 0
                      then Left (CompileError "field @size must be a multiple of its alignment" Nothing Nothing)
                      else Right sz
              let aligned = roundUp offset fieldAlign
              let entry = FieldLayout (textToString name) aligned fLayout fieldAlign fieldSize
              let offset' = aligned + fieldSize
              go offset' (entry:acc) cache1 rest
  in go 0 [] cache fields

parseFieldAlign :: [Attr] -> Either CompileError (Maybe Word32)
parseFieldAlign attrs = do
  mval <- attrSingleInt "align" attrs
  case mval of
    Nothing -> Right Nothing
    Just v ->
      if isPowerOfTwo v
        then Right (Just v)
        else Left (CompileError "@align must be a power of two" Nothing Nothing)

parseFieldSize :: [Attr] -> Either CompileError (Maybe Word32)
parseFieldSize = attrSingleInt "size"

attrSingleInt :: Text -> [Attr] -> Either CompileError (Maybe Word32)
attrSingleInt name attrs =
  case [args | Attr n args <- attrs, n == name] of
    [] -> Right Nothing
    [args] ->
      case args of
        [AttrInt v]
          | v <= 0 -> Left (CompileError ("@" <> textToString name <> " must be positive") Nothing Nothing)
          | v > fromIntegral (maxBound :: Word32) -> Left (CompileError ("@" <> textToString name <> " is out of range") Nothing Nothing)
          | otherwise -> Right (Just (fromIntegral v))
        _ -> Left (CompileError ("@" <> textToString name <> " expects a single integer argument") Nothing Nothing)
    _ -> Left (CompileError ("duplicate @" <> textToString name <> " attribute") Nothing Nothing)

isPowerOfTwo :: Word32 -> Bool
isPowerOfTwo v =
  v /= 0 && (v .&. (v - 1)) == 0

containsResource :: TypeLayout -> Bool
containsResource layout =
  case layout of
    TLSampler -> True
    TLSamplerComparison -> True
    TLTexture1D _ -> True
    TLTexture1DArray _ -> True
    TLTexture2D _ -> True
    TLTexture2DArray _ -> True
    TLTexture3D _ -> True
    TLTextureCube _ -> True
    TLTextureCubeArray _ -> True
    TLTextureMultisampled2D _ -> True
    TLTextureDepth2D -> True
    TLTextureDepth2DArray -> True
    TLTextureDepthCube -> True
    TLTextureDepthCubeArray -> True
    TLTextureDepthMultisampled2D -> True
    TLStorageTexture1D _ _ -> True
    TLStorageTexture2D _ _ -> True
    TLStorageTexture2DArray _ _ -> True
    TLStorageTexture3D _ _ -> True
    TLPointer _ _ elemLayout -> containsResource elemLayout
    TLArray _ _ elemLayout _ _ -> containsResource elemLayout
    TLMatrix {} -> False
    TLStruct _ fields _ _ -> any (containsResource . (.flType)) fields
    _ -> False

containsAtomic :: TypeLayout -> Bool
containsAtomic layout =
  case layout of
    TLAtomic _ -> True
    TLPointer _ _ elemLayout -> containsAtomic elemLayout
    TLArray _ _ elemLayout _ _ -> containsAtomic elemLayout
    TLStruct _ fields _ _ -> any (containsAtomic . (.flType)) fields
    _ -> False

containsPointer :: TypeLayout -> Bool
containsPointer layout =
  case layout of
    TLPointer {} -> True
    TLArray _ _ elemLayout _ _ -> containsPointer elemLayout
    TLStruct _ fields _ _ -> any (containsPointer . (.flType)) fields
    _ -> False

containsRuntimeArray :: TypeLayout -> Bool
containsRuntimeArray layout =
  case layout of
    TLArray Nothing _ _ _ _ -> True
    TLArray _ _ elemLayout _ _ -> containsRuntimeArray elemLayout
    TLStruct _ fields _ _ -> any (containsRuntimeArray . (.flType)) fields
    _ -> False

structSize :: [FieldLayout] -> Word32 -> Word32
structSize fields align =
  case fields of
    [] -> 0
    _ ->
      let lastField = last fields
          end = lastField.flOffset + lastField.flSize
      in roundUp end align

-- SPIR-V emission

emitSpirv :: CompileOptions -> ModuleAst -> ShaderInterface -> Either CompileError ByteString
emitSpirv opts modAst iface = do
  mEntry <- selectEntryPoint opts modAst.modEntries
  entry <- case mEntry of
    Nothing -> Left (CompileError "missing entry point" Nothing Nothing)
    Just e -> Right e
  validateEntry entry
  let structEnv = [(s.sdName, s) | s <- modAst.modStructs]
  structLayoutsMap <- resolveStructLayouts structEnv
  let structLayouts = Map.toList structLayoutsMap
  samplerLayouts <- collectSamplerLayouts structLayoutsMap (modAst.modBindings)
  retLayout <- case (entry.epStage, entry.epReturnType) of
    (StageFragment, Just ty) -> Just <$> resolveTypeLayoutWithCache structLayoutsMap ty
    (StageFragment, Nothing) -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)
    (StageVertex, Just ty) -> Just <$> resolveTypeLayoutWithCache structLayoutsMap ty
    (StageVertex, Nothing) -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)
    _ -> Right Nothing
  let blockStructs = [T.pack name | BindingInfo _ _ _ _ (TLStruct name _ _ _) <- iface.siBindings]
  let enabledFeatures = [feat | DirEnable feat <- modAst.modDirectives]
  let state0 = emptyGenState opts.samplerBindingMode entry.epStage structLayouts blockStructs samplerLayouts enabledFeatures
  let ((), state1) = emitStructs state0
  let node = ModuleNode "<merged>" [] modAst []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext [] "" node
  state2 <- emitModuleOverrides ctx constIndex fnIndex structIndex opts.overrideSpecMode structLayoutsMap structEnv (modAst.modOverrides) state1
  state3 <- emitModuleConstants (modAst.modConsts) state2
  (envGlobals, entryInits, _ifaceIds, outTargets, state4) <- emitGlobals structLayoutsMap structEnv iface entry retLayout (modAst.modGlobals) state3
  state5 <- registerFunctions structLayoutsMap (modAst.modFunctions) state4
  state6 <- emitFunctionBodies structLayoutsMap (modAst.modFunctions) state5
  state7 <- emitMainFunction entry envGlobals entryInits outTargets state6
  pure (buildSpirvBytes opts entry state7)

emitModuleOverrides :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideSpecMode -> StructLayoutCache -> [(Text, StructDecl)] -> [OverrideDecl] -> GenState -> Either CompileError GenState
emitModuleOverrides ctx constIndex fnIndex structIndex specMode layoutCache structEnv decls st0 =
  case decls of
    [] -> Right st0
    _ -> do
      specIds <- assignOverrideSpecIds decls
      let declMap = Map.fromList [(d.odName, d) | d <- decls]
      let depsMap = overrideDependencies decls
      order <- topoSortOverrides (map (.odName) decls) depsMap
      foldM (emitOne specIds depsMap) st0 (map (declMap Map.!) order)
  where
    emitOne specIds depsMap st decl = do
      ty <- maybe (Left (CompileError "override type could not be inferred" Nothing Nothing)) Right decl.odType
      layout <- resolveTypeLayoutWithCache layoutCache ty
      let deps = Map.findWithDefault Set.empty decl.odName depsMap
      (st2, val) <-
        maybe
          (defaultOverride layout ty st)
          (emitOverrideExpr deps layout st)
          decl.odExpr
      st3 <-
        if specMode == SpecParity || Set.null deps
          then do
            specId <- maybe (Left (CompileError "missing specialization id for override" Nothing Nothing)) Right
              (Map.lookup decl.odName specIds)
            Right (addDecoration (Instr opDecorate [val.valId, decorationSpecId, specId]) st2)
          else Right st2
      let st4 = addName (Instr opName (val.valId : encodeString (textToString decl.odName))) st3
      let st5 = st4
            { gsConstValues = (decl.odName, val) : st4.gsConstValues
            , gsConstValuesByName = Map.insert decl.odName val st4.gsConstValuesByName
            }
      Right st5

    fallbackLiteral layout expr st = do
      constVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr
      emitSpecConstValueToLayout layout constVal st

    defaultOverride layout ty st = do
      defaultVal <- defaultConstValueForType structEnv ty
      emitSpecConstValueToLayout layout defaultVal st

    emitOverrideExpr deps layout st expr =
      either
        (handleOverrideError deps layout expr st)
        (handleOverrideValue deps layout expr st)
        (emitSpecConstExpr ctx constIndex fnIndex structIndex st expr)

    handleOverrideValue deps layout expr st1 (st2, val0) = do
      (st3, val1) <-
        if val0.valType == layout
          then Right (st2, val0)
          else coerceSpecConstValueToLayout layout val0 st2
      if Set.null deps
        then
          if isSpecConstantLiteral val1.valId st3
            then Right (st3, val1)
            else fallbackLiteral layout expr st1
        else Right (st3, val1)

    handleOverrideError deps layout expr st err =
      if Set.null deps
        then fallbackLiteral layout expr st
        else
          Left
            ( CompileError
                ("override initializer depends on other overrides but is not a valid specialization-constant expression: " <> err.ceMessage)
                err.ceLine
                err.ceColumn
            )


defaultConstValueForType :: [(Text, StructDecl)] -> Type -> Either CompileError ConstValue
defaultConstValueForType structEnv ty =
  case ty of
    TyScalar Bool -> Right (CVBool False)
    TyScalar I32 -> Right (CVInt (ConstInt I32 0))
    TyScalar U32 -> Right (CVInt (ConstInt U32 0))
    TyScalar F32 -> Right (CVFloat (ConstFloat F32 0))
    TyScalar F16 -> Right (CVFloat (ConstFloat F16 0))
    TyVector n scalar -> do
      scalarVal <- defaultConstValueForType structEnv (TyScalar scalar)
      Right (CVVector n scalar (replicate n scalarVal))
    TyMatrix cols rows scalar -> do
      col <- defaultConstValueForType structEnv (TyVector rows scalar)
      Right (CVMatrix cols rows scalar (replicate cols col))
    TyArray elemTy (ArrayLenFixed n) -> do
      elemVal <- defaultConstValueForType structEnv elemTy
      Right (CVArray elemTy (replicate n elemVal))
    TyArray _ ArrayLenRuntime ->
      Left (CompileError "override defaults cannot target runtime-sized arrays" Nothing Nothing)
    TyArray _ (ArrayLenExpr _) ->
      Left (CompileError "override defaults require a fixed array length" Nothing Nothing)
    TyStructRef name ->
      case lookup name structEnv of
        Nothing -> Left (CompileError ("unknown struct: " <> textToString name) Nothing Nothing)
        Just decl -> do
          fields <- mapM fieldDefault (decl.sdFields)
          Right (CVStruct name fields)
    _ -> Left (CompileError "override defaults are only supported for scalar, vector, matrix, array, and struct types" Nothing Nothing)
  where
    fieldDefault fld = do
      val <- defaultConstValueForType structEnv fld.fdType
      Right (fld.fdName, val)

emitModuleConstants :: [ConstDecl] -> GenState -> Either CompileError GenState
emitModuleConstants decls st0 = foldM emitOne st0 decls
  where
    emitOne st decl =
      maybe
        (do
            (st1, val) <- emitConstExpr st decl.cdExpr
            (st2, val') <-
              case decl.cdType of
                Nothing -> Right (st1, val)
                Just ty -> do
                  layout <- resolveTypeLayoutInState st1 ty
                  coerceConstValueToLayout layout val st1
            let st3 = st2
                  { gsConstValues = (decl.cdName, val') : st2.gsConstValues
                  , gsConstValuesByName = Map.insert decl.cdName val' st2.gsConstValuesByName
                  }
            pure st3
        )
        (const (Left (CompileError ("duplicate constant: " <> textToString decl.cdName) Nothing Nothing)))
        (Map.lookup decl.cdName st.gsConstValuesByName)

emitConstExpr :: GenState -> Expr -> Either CompileError (GenState, Value)
emitConstExpr st expr =
  case expr of
    EInt _ n -> do
      (scalar, val) <- selectIntLiteralScalar n
      case scalar of
        I32 -> do
          let (cid, st1) = emitConstI32 st (fromIntegral val)
          let (a, sz) = scalarLayout I32
          let layout = TLScalar I32 a sz
          Right (st1, Value layout cid)
        U32 -> do
          let (cid, st1) = emitConstU32 st (fromIntegral val)
          let (a, sz) = scalarLayout U32
          let layout = TLScalar U32 a sz
          Right (st1, Value layout cid)
        _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
    EFloat _ f -> do
      let (cid, st1) = emitConstF32 st f
      let (a, sz) = scalarLayout F32
      let layout = TLScalar F32 a sz
      Right (st1, Value layout cid)
    EBool _ b -> do
      let (cid, st1) = emitConstBool st b
      let (a, sz) = scalarLayout Bool
      let layout = TLScalar Bool a sz
      Right (st1, Value layout cid)
    EUnary _ OpNeg inner -> do
      (st1, val) <- emitConstExpr st inner
      case val.valType of
        TLScalar I32 _ _ -> do
          key <- maybe (Left (CompileError "constant integer literal required" Nothing Nothing)) Right (lookupConstKeyById st1 (val.valId))
          v <- constKeyToI32 key
          let out = negate (fromIntegral v :: Integer)
          (cid, st2) <- emitConstIntScalar I32 out st1
          let (a, sz) = scalarLayout I32
          let layout = TLScalar I32 a sz
          Right (st2, Value layout cid)
        TLScalar U32 _ _ -> Left (CompileError "unary minus is not supported for u32 constants" Nothing Nothing)
        TLScalar F32 _ _ -> do
          key <- maybe (Left (CompileError "constant float literal required" Nothing Nothing)) Right (lookupConstKeyById st1 (val.valId))
          v <- constKeyToFloat key
          let (cid, st2) = emitConstF32 st1 (negate v)
          let (a, sz) = scalarLayout F32
          let layout = TLScalar F32 a sz
          Right (st2, Value layout cid)
        TLScalar F16 _ _ -> do
          key <- maybe (Left (CompileError "constant float literal required" Nothing Nothing)) Right (lookupConstKeyById st1 (val.valId))
          v <- constKeyToFloat key
          let (cid, st2) = emitConstF16 st1 (negate v)
          let (a, sz) = scalarLayout F16
          let layout = TLScalar F16 a sz
          Right (st2, Value layout cid)
        _ -> Left (CompileError "unary minus expects a numeric constant" Nothing Nothing)
    EBinary _ op a b -> do
      (st1, v1) <- emitConstExpr st a
      (st2, v2) <- emitConstExpr st1 b
      case (v1.valType, v2.valType) of
        (TLScalar s1 _ _, TLScalar s2 _ _) ->
          case (s1, s2) of
            (F32, F32) -> emitFloatBin op F32 st2 v1 v2
            (F16, F16) -> emitFloatBin op F16 st2 v1 v2
            (F32, F16) -> emitFloatBin op F32 st2 v1 v2
            (F16, F32) -> emitFloatBin op F32 st2 v1 v2
            _ -> emitIntBin op s1 s2 st2 v1 v2
        _ -> Left (CompileError "constant operation expects scalar values" Nothing Nothing)
    EVar _ name ->
      case Map.lookup name st.gsConstValuesByName of
        Just val -> Right (st, val)
        Nothing -> Left (CompileError ("unknown constant: " <> textToString name) Nothing Nothing)
    ECall _ name args ->
      case parseVectorCtorName name of
        Just (n, targetScalar) -> emitConstVectorCtor n targetScalar args st
        Nothing ->
          case parseArrayCtorName name of
            Just (elemTy, arrLen) -> emitConstTypedArrayCtor elemTy arrLen args st
            Nothing ->
              case name of
                "array" -> emitConstArrayCtor args st
                "f16" -> emitConstScalarCtor F16 args st
                "f32" -> emitConstScalarCtor F32 args st
                "u32" -> emitConstScalarCtor U32 args st
                "i32" -> emitConstScalarCtor I32 args st
                _ ->
                  case parseMatrixCtorName name of
                    Just (cols, rows, targetScalar) -> emitConstMatrixCtor cols rows targetScalar args st
                    Nothing ->
                      case lookup name (st.gsStructLayouts) of
                        Just layout -> emitConstStructCtor name layout args st
                        Nothing -> Left (CompileError ("unsupported constant constructor: " <> textToString name) Nothing Nothing)
    _ -> Left (CompileError "unsupported constant expression" Nothing Nothing)
  where
    emitFloatBin op' target st' v1 v2 = do
      key1 <- maybe (Left (CompileError "constant float literal required" Nothing Nothing)) Right (lookupConstKeyById st' (v1.valId))
      key2 <- maybe (Left (CompileError "constant float literal required" Nothing Nothing)) Right (lookupConstKeyById st' (v2.valId))
      x <- constKeyToFloat key1
      y <- constKeyToFloat key2
      out <- case op' of
        OpAdd -> pure (x + y)
        OpSub -> pure (x - y)
        OpMul -> pure (x * y)
        OpDiv ->
          if y == 0 then Left (CompileError "division by zero in constant expression" Nothing Nothing) else pure (x / y)
        _ -> Left (CompileError "unsupported constant float operation" Nothing Nothing)
      case target of
        F32 -> do
          let (cid, st3) = emitConstF32 st' out
          let (a, sz) = scalarLayout F32
          Right (st3, Value (TLScalar F32 a sz) cid)
        F16 -> do
          let (cid, st3) = emitConstF16 st' out
          let (a, sz) = scalarLayout F16
          Right (st3, Value (TLScalar F16 a sz) cid)
        _ -> Left (CompileError "unsupported constant float operation" Nothing Nothing)
    emitIntBin op' s1 s2 st' v1 v2 = do
      key1 <- maybe (Left (CompileError "constant integer literal required" Nothing Nothing)) Right (lookupConstKeyById st' (v1.valId))
      key2 <- maybe (Left (CompileError "constant integer literal required" Nothing Nothing)) Right (lookupConstKeyById st' (v2.valId))
      i1 <- constKeyToInt key1
      i2 <- constKeyToInt key2
      let (scalar, x, y) = coerceConstPair s1 i1 s2 i2
      out <- case op' of
        OpAdd -> pure (x + y)
        OpSub -> pure (x - y)
        OpMul -> pure (x * y)
        OpDiv ->
          if y == 0 then Left (CompileError "division by zero in constant expression" Nothing Nothing) else pure (x `quot` y)
        OpMod ->
          if y == 0 then Left (CompileError "modulo by zero in constant expression" Nothing Nothing) else pure (x `rem` y)
        OpBitAnd -> pure (x .&. y)
        OpBitOr -> pure (x .|. y)
        OpBitXor -> pure (xor x y)
        OpShl ->
          if y < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else pure (shiftL x (fromIntegral y))
        OpShr ->
          if y < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else pure (shiftR x (fromIntegral y))
        _ -> Left (CompileError "unsupported constant integer operation" Nothing Nothing)
      case scalar of
        I32 -> do
          let out' = out
          when (out' < 0 || out' > 0x7FFFFFFF) $
            Left (CompileError "constant i32 is out of range" Nothing Nothing)
          (cid, st3) <- emitConstIntScalar I32 out' st'
          let (a', sz') = scalarLayout I32
          Right (st3, Value (TLScalar I32 a' sz') cid)
        U32 -> do
          let out' = out
          when (out' < 0 || out' > fromIntegral (maxBound :: Word32)) $
            Left (CompileError "constant u32 is out of range" Nothing Nothing)
          (cid, st3) <- emitConstIntScalar U32 out' st'
          let (a', sz') = scalarLayout U32
          Right (st3, Value (TLScalar U32 a' sz') cid)
        _ -> Left (CompileError "unsupported constant integer operation" Nothing Nothing)
    constKeyToInt key =
      case key of
        ConstI32 v -> Right (fromIntegral (fromIntegral v :: Int32))
        ConstU32 v -> Right (fromIntegral v)
        _ -> Left (CompileError "expected integer constant" Nothing Nothing)
    coerceConstPair s1 v1 s2 v2 =
      case (s1, s2) of
        (I32, I32) -> (I32, v1, v2)
        (U32, U32) -> (U32, v1, v2)
        (I32, U32) -> (I32, v1, v2)
        (U32, I32) -> (I32, v1, v2)
        _ -> (I32, v1, v2)

emitConstValueToLayout :: TypeLayout -> ConstValue -> GenState -> Either CompileError (GenState, Value)
emitConstValueToLayout layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      val' <- coerceConstScalarValue scalar val
      case val' of
        CVBool b ->
          let (cid, st1) = emitConstBool st b
          in Right (st1, Value layout cid)
        CVInt (ConstInt _ v) -> do
          (cid, st1) <- emitConstIntScalar scalar v st
          Right (st1, Value layout cid)
        CVFloat (ConstFloat _ v) -> do
          (cid, st1) <- emitConstFloatScalar scalar (realToFrac v) st
          Right (st1, Value layout cid)
        _ -> Left (CompileError "expected scalar constant" Nothing Nothing)
    TLVector n scalar _ _ ->
      case val of
        CVVector m _ comps | m == n -> do
          comps' <- mapM (coerceConstScalarValue scalar) comps
          let (a, sz) = scalarLayout scalar
          (st1, vals) <- emitConstValues (TLScalar scalar a sz) comps' st
          let (cid, st2) = emitConstComposite layout (map (.valId) vals) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "vector constant does not match type" Nothing Nothing)
    TLMatrix cols rows scalar _ _ _ ->
      case val of
        CVMatrix c r _ colsVals | c == cols && r == rows -> do
          let (a, sz) = vectorLayout scalar rows
          let colLayout = TLVector rows scalar a sz
          (st1, colsEmitted) <- emitConstValues colLayout colsVals st
          let (cid, st2) = emitConstComposite layout (map (.valId) colsEmitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "matrix constant does not match type" Nothing Nothing)
    TLArray mlen _ elemLayout _ _ ->
      case (mlen, val) of
        (Just n, CVArray _ elems) | length elems == n -> do
          (st1, vals) <- emitConstValues elemLayout elems st
          let (cid, st2) = emitConstComposite layout (map (.valId) vals) st1
          Right (st2, Value layout cid)
        (Nothing, _) -> Left (CompileError "runtime array constants are not supported" Nothing Nothing)
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TLStruct name fields _ _ ->
      let nameT = T.pack name
      in case val of
        CVStruct structName pairs | structName == nameT -> do
          fieldVals <- mapM (lookupField pairs) fields
          (st1, emitted) <- emitConstValuesFromFields fields fieldVals st
          let (cid, st2) = emitConstComposite layout (map (.valId) emitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "struct constant does not match type" Nothing Nothing)
    _ -> Left (CompileError "unsupported constant layout" Nothing Nothing)
  where
    lookupField pairs fld =
      case lookup (T.pack fld.flName) pairs of
        Just v -> Right v
        Nothing -> Left (CompileError ("missing field: " <> fld.flName) Nothing Nothing)

    emitConstValues elemLayout vals st0 = do
      (st1, revVals) <- foldM go (st0, []) vals
      Right (st1, reverse revVals)
      where
        go (stAcc, acc) v = do
          (st', val') <- emitConstValueToLayout elemLayout v stAcc
          Right (st', val' : acc)

    emitConstValuesFromFields fieldLayouts vals st0 = do
      (st1, revVals) <- foldM go (st0, []) (zip fieldLayouts vals)
      Right (st1, reverse revVals)
      where
        go (stAcc, acc) (fld, v) = do
          (st', val') <- emitConstValueToLayout fld.flType v stAcc
          Right (st', val' : acc)

emitSpecConstValueToLayout :: TypeLayout -> ConstValue -> GenState -> Either CompileError (GenState, Value)
emitSpecConstValueToLayout layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      val' <- coerceConstScalarValue scalar val
      case val' of
        CVBool b ->
          let (cid, st1) = emitSpecConstBool st b
          in Right (st1, Value layout cid)
        CVInt (ConstInt _ v) -> do
          (cid, st1) <- emitSpecConstIntScalar scalar v st
          Right (st1, Value layout cid)
        CVFloat (ConstFloat _ v) -> do
          (cid, st1) <- emitSpecConstFloatScalar scalar (realToFrac v) st
          Right (st1, Value layout cid)
        _ -> Left (CompileError "expected scalar constant" Nothing Nothing)
    TLVector n scalar _ _ ->
      case val of
        CVVector m _ comps | m == n -> do
          comps' <- mapM (coerceConstScalarValue scalar) comps
          let (a, sz) = scalarLayout scalar
          (st1, vals) <- emitConstValues (TLScalar scalar a sz) comps' st
          let (cid, st2) = emitSpecConstComposite layout (map (.valId) vals) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "vector constant does not match type" Nothing Nothing)
    TLMatrix cols rows scalar _ _ _ ->
      case val of
        CVMatrix c r _ colsVals | c == cols && r == rows -> do
          let (a, sz) = vectorLayout scalar rows
          let colLayout = TLVector rows scalar a sz
          (st1, colsEmitted) <- emitConstValues colLayout colsVals st
          let (cid, st2) = emitSpecConstComposite layout (map (.valId) colsEmitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "matrix constant does not match type" Nothing Nothing)
    TLArray mlen _ elemLayout _ _ ->
      case (mlen, val) of
        (Just n, CVArray _ elems) | length elems == n -> do
          (st1, vals) <- emitConstValues elemLayout elems st
          let (cid, st2) = emitSpecConstComposite layout (map (.valId) vals) st1
          Right (st2, Value layout cid)
        (Nothing, _) -> Left (CompileError "runtime array constants are not supported" Nothing Nothing)
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TLStruct name fields _ _ ->
      let nameT = T.pack name
      in case val of
        CVStruct structName pairs | structName == nameT -> do
          fieldVals <- mapM (lookupField pairs) fields
          (st1, emitted) <- emitConstValuesFromFields fields fieldVals st
          let (cid, st2) = emitSpecConstComposite layout (map (.valId) emitted) st1
          Right (st2, Value layout cid)
        _ -> Left (CompileError "struct constant does not match type" Nothing Nothing)
    _ -> Left (CompileError "unsupported constant layout" Nothing Nothing)
  where
    lookupField pairs fld =
      case lookup (T.pack fld.flName) pairs of
        Just v -> Right v
        Nothing -> Left (CompileError ("missing field: " <> fld.flName) Nothing Nothing)

    emitConstValues elemLayout vals st0 = do
      (st1, revVals) <- foldM go (st0, []) vals
      Right (st1, reverse revVals)
      where
        go (stAcc, acc) v = do
          (st', val') <- emitConstValueToLayout elemLayout v stAcc
          Right (st', val' : acc)

    emitConstValuesFromFields fieldLayouts vals st0 = do
      (st1, revVals) <- foldM go (st0, []) (zip fieldLayouts vals)
      Right (st1, reverse revVals)
      where
        go (stAcc, acc) (fld, v) = do
          (st', val') <- emitConstValueToLayout fld.flType v stAcc
          Right (st', val' : acc)

constValueLayout :: GenState -> ConstValue -> Either CompileError TypeLayout
constValueLayout st val =
  case val of
    CVBool _ ->
      let (a, sz) = scalarLayout Bool
      in Right (TLScalar Bool a sz)
    CVInt (ConstInt scalar _) ->
      let (a, sz) = scalarLayout scalar
      in Right (TLScalar scalar a sz)
    CVFloat (ConstFloat scalar _) ->
      let (a, sz) = scalarLayout scalar
      in Right (TLScalar scalar a sz)
    CVVector n scalar _ ->
      let (a, sz) = vectorLayout scalar n
      in Right (TLVector n scalar a sz)
    CVMatrix cols rows scalar _ ->
      let layout = matrixLayout cols rows scalar
      in Right layout
    CVArray elemTy elems -> do
      elemLayout <- typeLayoutFromValue elemTy
      let elemAlign = layoutAlign elemLayout
      let elemSize = layoutSize elemLayout
      let stride = roundUp elemSize elemAlign
      let total = stride * fromIntegral (length elems)
      Right (TLArray (Just (length elems)) stride elemLayout elemAlign total)
    CVStruct name _ ->
      case lookup name (st.gsStructLayouts) of
        Just layout -> Right layout
        Nothing -> Left (CompileError ("unknown struct layout for constant: " <> textToString name) Nothing Nothing)
    CVPointer _ _ ->
      Left (CompileError "pointer constants are not supported" Nothing Nothing)
  where
    typeLayoutFromValue ty =
      case ty of
        TyScalar scalar ->
          let (a, sz) = scalarLayout scalar
          in Right (TLScalar scalar a sz)
        TyVector n scalar ->
          let (a, sz) = vectorLayout scalar n
          in Right (TLVector n scalar a sz)
        TyMatrix cols rows scalar ->
          Right (matrixLayout cols rows scalar)
        TyArray elemTy (ArrayLenFixed count) -> do
          elemLayout <- typeLayoutFromValue elemTy
          let elemAlign = layoutAlign elemLayout
          let elemSize = layoutSize elemLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral count
          Right (TLArray (Just count) stride elemLayout elemAlign total)
        TyArray _ ArrayLenRuntime ->
          Left (CompileError "runtime array constants are not supported" Nothing Nothing)
        TyArray _ (ArrayLenExpr _) ->
          Left (CompileError "array length expressions must be resolved before layout" Nothing Nothing)
        TyStructRef name ->
          case lookup name (st.gsStructLayouts) of
            Just layout -> Right layout
            Nothing -> Left (CompileError ("unknown struct layout for constant: " <> textToString name) Nothing Nothing)
        _ -> Left (CompileError "unsupported constant type layout" Nothing Nothing)

emitSpecConstOp :: TypeLayout -> Word16 -> [Word32] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstOp layout opcode operands st = do
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let instr = Instr opSpecConstantOp (tyId : resId : fromIntegral opcode : operands)
  let st3 = addConst instr st2
  Right (st3, Value layout resId)

isSpecConstantLiteral :: Word32 -> GenState -> Bool
isSpecConstantLiteral cid st = Set.member cid st.gsSpecConstLiteralIds

emitSpecConstScalarConvert :: Scalar -> Scalar -> Value -> GenState -> Either CompileError (GenState, Value)
emitSpecConstScalarConvert fromScalar toScalarTy val st = do
  opcode <- case (fromScalar, toScalarTy) of
    (U32, F32) -> Right opConvertUToF
    (I32, F32) -> Right opConvertSToF
    (U32, F16) -> Right opConvertUToF
    (I32, F16) -> Right opConvertSToF
    (F32, U32) -> Right opConvertFToU
    (F32, I32) -> Right opConvertFToS
    (F16, U32) -> Right opConvertFToU
    (F16, I32) -> Right opConvertFToS
    (F16, F32) -> Right opFConvert
    (F32, F16) -> Right opFConvert
    (U32, I32) -> Right opBitcast
    (I32, U32) -> Right opBitcast
    _ -> Left (CompileError "unsupported scalar conversion for spec constant" Nothing Nothing)
  let (a, sz) = scalarLayout toScalarTy
  let layout = TLScalar toScalarTy a sz
  emitSpecConstOp layout opcode [val.valId] st

coerceSpecConstValueToLayout :: TypeLayout -> Value -> GenState -> Either CompileError (GenState, Value)
coerceSpecConstValueToLayout target val st
  | val.valType == target = Right (st, val)
  | otherwise =
      case (target, val.valType) of
        (TLScalar targetScalar _ _, TLScalar srcScalar _ _) -> do
          case targetScalar of
            Bool ->
              Left (CompileError "implicit conversion to bool is not supported for spec constants" Nothing Nothing)
            _ ->
              emitSpecConstScalarConvert srcScalar targetScalar val st
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceSpecConstValuesToLayout :: TypeLayout -> [Value] -> GenState -> Either CompileError (GenState, [Value])
coerceSpecConstValuesToLayout target vals st = go st [] vals
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (v:vs) = do
      (st1, v') <- coerceSpecConstValueToLayout target v st'
      go st1 (v':acc) vs

emitSpecConstExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> GenState -> Expr -> Either CompileError (GenState, Value)
emitSpecConstExpr ctx constIndex fnIndex structIndex st expr =
  either
    (const fallback)
    Right
    (tryEmit st expr)
  where
    fallback = do
      constVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr
      layout <- constValueLayout st constVal
      emitSpecConstValueToLayout layout constVal st
    tryEmit st0 ex =
      case ex of
        EInt _ n -> do
          (scalar, val) <- selectIntLiteralScalar n
          case scalar of
            I32 -> do
              (cid, st1) <- emitSpecConstIntScalar I32 val st0
              let (a, sz) = scalarLayout I32
              Right (st1, Value (TLScalar I32 a sz) cid)
            U32 -> do
              (cid, st1) <- emitSpecConstIntScalar U32 val st0
              let (a, sz) = scalarLayout U32
              Right (st1, Value (TLScalar U32 a sz) cid)
            _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
        EFloat _ f -> do
          (cid, st1) <- emitSpecConstFloatScalar F32 f st0
          let (a, sz) = scalarLayout F32
          Right (st1, Value (TLScalar F32 a sz) cid)
        EBool _ b -> do
          let (cid, st1) = emitSpecConstBool st0 b
          let (a, sz) = scalarLayout Bool
          Right (st1, Value (TLScalar Bool a sz) cid)
        EUnary _ OpNeg inner -> do
          (st1, v) <- tryEmit st0 inner
          case v.valType of
            TLScalar I32 _ _ -> emitSpecConstOp (v.valType) opSNegate [v.valId] st1
            TLScalar U32 _ _ -> Left (CompileError "unary minus is not supported for u32 spec constants" Nothing Nothing)
            TLScalar F32 _ _ -> emitSpecConstOp (v.valType) opFNegate [v.valId] st1
            TLScalar F16 _ _ -> emitSpecConstOp (v.valType) opFNegate [v.valId] st1
            _ -> Left (CompileError "unary minus expects a scalar" Nothing Nothing)
        EUnary _ OpNot inner -> do
          (st1, v) <- tryEmit st0 inner
          case v.valType of
            TLScalar Bool _ _ -> emitSpecConstOp (v.valType) opLogicalNot [v.valId] st1
            _ -> Left (CompileError "logical not expects bool" Nothing Nothing)
        EBinary _ op a b -> do
          (st1, v1) <- tryEmit st0 a
          (st2, v2) <- tryEmit st1 b
          case (v1.valType, v2.valType) of
            (TLScalar s1 _ _, TLScalar s2 _ _) | s1 == s2 -> do
              let layout = v1.valType
              let opIds = [v1.valId, v2.valId]
              case s1 of
                I32 ->
                  emitSpecConstIntOp layout op opIds st2
                U32 ->
                  emitSpecConstIntOp layout op opIds st2
                F32 ->
                  emitSpecConstFloatOp layout op opIds st2
                F16 ->
                  emitSpecConstFloatOp layout op opIds st2
                Bool ->
                  emitSpecConstBoolOp layout op opIds st2
            _ -> Left (CompileError "unsupported spec constant binary operation" Nothing Nothing)
        ECall _ "select" [aExpr, bExpr, condExpr] -> do
          (st1, vA) <- tryEmit st0 aExpr
          (st2, vB) <- tryEmit st1 bExpr
          (st3, vCond) <- tryEmit st2 condExpr
          case (vA.valType, vB.valType, vCond.valType) of
            (layoutA, layoutB, TLScalar Bool _ _) | layoutA == layoutB ->
              emitSpecConstOp layoutA opSelect [vCond.valId, vA.valId, vB.valId] st3
            _ -> Left (CompileError "select requires matching value types and a bool condition" Nothing Nothing)
        ECall _ name args ->
          case parseVectorCtorName name of
            Just (n, targetScalar) ->
              emitSpecConstVectorCtor ctx constIndex fnIndex structIndex n targetScalar args st0
            Nothing ->
              case parseArrayCtorName name of
                Just (elemTy, arrLen) ->
                  emitSpecConstTypedArrayCtor ctx constIndex fnIndex structIndex elemTy arrLen args st0
                Nothing ->
                  case name of
                    "array" -> emitSpecConstArrayCtor ctx constIndex fnIndex structIndex args st0
                    "f16" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex F16 args st0
                    "f32" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex F32 args st0
                    "u32" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex U32 args st0
                    "i32" -> emitSpecConstScalarCtor ctx constIndex fnIndex structIndex I32 args st0
                    _ ->
                      case parseMatrixCtorName name of
                        Just (cols, rows, targetScalar) ->
                          emitSpecConstMatrixCtor ctx constIndex fnIndex structIndex cols rows targetScalar args st0
                        Nothing ->
                          case lookup name (st.gsStructLayouts) of
                            Just layout -> emitSpecConstStructCtor ctx constIndex fnIndex structIndex name layout args st0
                            Nothing -> Left (CompileError ("unsupported spec constant constructor: " <> textToString name) Nothing Nothing)
        EVar _ name ->
          case Map.lookup name st0.gsConstValuesByName of
            Just val -> Right (st0, val)
            Nothing -> Left (CompileError "const reference requires fallback" Nothing Nothing)
        _ -> Left (CompileError "unsupported spec constant expression" Nothing Nothing)

    emitSpecConstIntOp layout op opIds st' =
      case op of
        OpAdd -> emitSpecConstOp layout opIAdd opIds st'
        OpSub -> emitSpecConstOp layout opISub opIds st'
        OpMul -> emitSpecConstOp layout opIMul opIds st'
        OpDiv ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp layout opUDiv opIds st'
            _ -> emitSpecConstOp layout opSDiv opIds st'
        OpMod ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp layout opUMod opIds st'
            _ -> emitSpecConstOp layout opSRem opIds st'
        OpBitAnd -> emitSpecConstOp layout opBitwiseAnd opIds st'
        OpBitOr -> emitSpecConstOp layout opBitwiseOr opIds st'
        OpBitXor -> emitSpecConstOp layout opBitwiseXor opIds st'
        OpShl -> emitSpecConstOp layout opShiftLeftLogical opIds st'
        OpShr ->
          case layout of
            TLScalar I32 _ _ -> emitSpecConstOp layout opShiftRightArithmetic opIds st'
            _ -> emitSpecConstOp layout opShiftRightLogical opIds st'
        OpEq -> emitSpecConstOp boolLayout opIEqual opIds st'
        OpNe -> emitSpecConstOp boolLayout opINotEqual opIds st'
        OpLt ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opULessThan opIds st'
            _ -> emitSpecConstOp boolLayout opSLessThan opIds st'
        OpLe ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opULessThanEqual opIds st'
            _ -> emitSpecConstOp boolLayout opSLessThanEqual opIds st'
        OpGt ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opUGreaterThan opIds st'
            _ -> emitSpecConstOp boolLayout opSGreaterThan opIds st'
        OpGe ->
          case layout of
            TLScalar U32 _ _ -> emitSpecConstOp boolLayout opUGreaterThanEqual opIds st'
            _ -> emitSpecConstOp boolLayout opSGreaterThanEqual opIds st'
        _ -> Left (CompileError "unsupported int spec constant op" Nothing Nothing)

    emitSpecConstFloatOp layout op opIds st' =
      case op of
        OpAdd -> emitSpecConstOp layout opFAdd opIds st'
        OpSub -> emitSpecConstOp layout opFSub opIds st'
        OpMul -> emitSpecConstOp layout opFMul opIds st'
        OpDiv -> emitSpecConstOp layout opFDiv opIds st'
        OpEq -> emitSpecConstOp boolLayout opFOrdEqual opIds st'
        OpNe -> emitSpecConstOp boolLayout opFOrdNotEqual opIds st'
        OpLt -> emitSpecConstOp boolLayout opFOrdLessThan opIds st'
        OpLe -> emitSpecConstOp boolLayout opFOrdLessThanEqual opIds st'
        OpGt -> emitSpecConstOp boolLayout opFOrdGreaterThan opIds st'
        OpGe -> emitSpecConstOp boolLayout opFOrdGreaterThanEqual opIds st'
        _ -> Left (CompileError "unsupported float spec constant op" Nothing Nothing)

    emitSpecConstBoolOp layout op opIds st' =
      case op of
        OpAnd -> emitSpecConstOp layout opLogicalAnd opIds st'
        OpOr -> emitSpecConstOp layout opLogicalOr opIds st'
        OpEq -> emitSpecConstOp layout opLogicalEqual opIds st'
        OpNe -> emitSpecConstOp layout opLogicalNotEqual opIds st'
        _ -> Left (CompileError "unsupported bool spec constant op" Nothing Nothing)

    boolLayout =
      let (a, sz) = scalarLayout Bool
      in TLScalar Bool a sz

emitSpecConstExprList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> GenState -> [Expr] -> Either CompileError (GenState, [Value])
emitSpecConstExprList ctx constIndex fnIndex structIndex st = go st []
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (e:es) = do
      (st1, v) <- emitSpecConstExpr ctx constIndex fnIndex structIndex st' e
      go st1 (v:acc) es

emitSpecConstVectorCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Int -> Maybe Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstVectorCtor ctx constIndex fnIndex structIndex n targetScalar args st =
  if null args
    then
      case targetScalar of
        Nothing -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        Just scalar -> do
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          emitZeroSpecConstValue layout st
    else do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      let singleScalar =
            case vals of
              [v] ->
                case v.valType of
                  TLScalar {} -> True
                  _ -> False
              _ -> False
      flattened <- flattenConstScalars st1 vals
      case flattened of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = case targetScalar of
                Just s ->
                  let (a, sz) = scalarLayout s
                  in TLScalar s a sz
                Nothing -> pickBaseLayout st1 flattened
          (st2, vals') <- coerceSpecConstValuesToLayout baseLayout flattened st1
          scalar <- case baseLayout of
            TLScalar s _ _ -> Right s
            _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
          let filled =
                case (singleScalar, vals') of
                  (True, [v]) -> replicate n v
                  _ -> vals'
          when (length filled /= n) $
            Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          let (cid, st3) = emitSpecConstComposite layout (map (.valId) filled) st2
          Right (st3, Value layout cid)

emitSpecConstMatrixCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Int -> Int -> Maybe Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstMatrixCtor ctx constIndex fnIndex structIndex cols rows targetScalar args st =
  if null args
    then
      case targetScalar of
        Nothing -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        Just scalar ->
          emitZeroSpecConstValue (matrixLayout cols rows scalar) st
    else do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      case vals of
        [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        [v] | isScalarLayout v.valType -> do
          (st2, scalarVal, scalar) <- coerceSingleScalar v st1
          (st3, zeroVal) <- emitSpecConstZeroScalar scalar st2
          let (va, vsz) = vectorLayout scalar rows
          let vecLayout = TLVector rows scalar va vsz
          (st4, colsVals) <- buildDiagColumns vecLayout scalarVal zeroVal st3
          let layout = matrixLayout cols rows scalar
          let (cid, st5) = emitSpecConstComposite layout (map (.valId) colsVals) st4
          Right (st5, Value layout cid)
        _ -> do
          let scalarCount = cols * rows
          flattened <- flattenMatrixArgs rows vals st1
          case flattened of
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
            _ -> do
              let baseLayout = case targetScalar of
                    Just s ->
                      let (a, sz) = scalarLayout s
                      in TLScalar s a sz
                    Nothing -> pickBaseLayout st1 flattened
              (st2, vals') <- coerceSpecConstValuesToLayout baseLayout flattened st1
              scalar <- case baseLayout of
                TLScalar s _ _ -> Right s
                _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
              when (length vals' /= scalarCount) $
                Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
              let (va, vsz) = vectorLayout scalar rows
              let vecLayout = TLVector rows scalar va vsz
              (st3, colsVals) <- buildColumns vecLayout vals' st2
              let layout = matrixLayout cols rows scalar
              let (cid, st4) = emitSpecConstComposite layout (map (.valId) colsVals) st3
              Right (st4, Value layout cid)
  where
    isScalarLayout layout =
      case layout of
        TLScalar {} -> True
        _ -> False
    coerceSingleScalar v st' = do
      let layout = v.valType
      case targetScalar of
        Just s -> do
          let (a, sz) = scalarLayout s
          (st1, vals') <- coerceSpecConstValuesToLayout (TLScalar s a sz) [v] st'
          case vals' of
            [v'] -> Right (st1, v', s)
            _ -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        Nothing ->
          case layout of
            TLScalar s _ _ -> Right (st', v, s)
            _ -> Left (CompileError "matrix constructor expects a scalar" Nothing Nothing)
    emitSpecConstZeroScalar scalar st' =
      case scalar of
        F16 -> do
          (cid, st1) <- emitSpecConstFloatScalar F16 0 st'
          let (a, sz) = scalarLayout F16
          Right (st1, Value (TLScalar F16 a sz) cid)
        F32 -> do
          (cid, st1) <- emitSpecConstFloatScalar F32 0 st'
          let (a, sz) = scalarLayout F32
          Right (st1, Value (TLScalar F32 a sz) cid)
        I32 -> do
          (cid, st1) <- emitSpecConstIntScalar I32 0 st'
          let (a, sz) = scalarLayout I32
          Right (st1, Value (TLScalar I32 a sz) cid)
        U32 -> do
          (cid, st1) <- emitSpecConstIntScalar U32 0 st'
          let (a, sz) = scalarLayout U32
          Right (st1, Value (TLScalar U32 a sz) cid)
        Bool -> do
          let (cid, st1) = emitSpecConstBool st' False
          let (a, sz) = scalarLayout Bool
          Right (st1, Value (TLScalar Bool a sz) cid)
    flattenMatrixArgs rows' vals st' = fmap concat (mapM (flattenOne st') vals)
      where
        flattenOne st'' v =
          case v.valType of
            TLScalar {} -> Right [v]
            TLVector n _ _ _
              | n == rows' -> flattenConstScalars st'' [v]
              | otherwise ->
                  Left (CompileError "matrix constructor expects column vectors matching the row count" Nothing Nothing)
            _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
    buildDiagColumns vecLayout diagVal zeroVal st' =
      go 0 st'
      where
        go colIx st1
          | colIx >= cols = Right (st1, [])
          | otherwise = do
              let elems =
                    [ if rowIx == colIx then diagVal else zeroVal
                    | rowIx <- [0 .. rows - 1]
                    ]
              let (cid, st2) = emitSpecConstComposite vecLayout (map (.valId) elems) st1
              (st3, rest) <- go (colIx + 1) st2
              Right (st3, Value vecLayout cid : rest)
    buildColumns vecLayout vals st' =
      case splitAt rows vals of
        ([], _) -> Right (st', [])
        (col, rest) -> do
          let (cid, st1) = emitSpecConstComposite vecLayout (map (.valId) col) st'
          (st2, colsVals) <- buildColumns vecLayout rest st1
          Right (st2, Value vecLayout cid : colsVals)

emitSpecConstStructCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Text -> TypeLayout -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstStructCtor ctx constIndex fnIndex structIndex name layout args st =
  case layout of
    TLStruct _ fields _ _ -> do
      case args of
        [] -> emitZeroSpecConstValue layout st
        _ -> do
          when (length args /= length fields) $
            Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
          (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
          (st2, vals') <- coerceSpecConstArgsToLayouts vals (map (.flType) fields) st1
          let (cid, st3) = emitSpecConstComposite layout (map (.valId) vals') st2
          Right (st3, Value layout cid)
    _ -> Left (CompileError ("unsupported constructor: " <> textToString name) Nothing Nothing)

emitSpecConstArrayCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstArrayCtor ctx constIndex fnIndex structIndex args st =
  case args of
    [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
    _ -> do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        _ -> do
          let firstLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceSpecConstValuesToLayout firstLayout vals st1
          when (containsResource firstLayout) $
            Left (CompileError "arrays of resources are not supported" Nothing Nothing)
          when (containsAtomic firstLayout) $
            Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
          let elemAlign = layoutAlign firstLayout
          let elemSize = layoutSize firstLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral (length vals')
          let layout = TLArray (Just (length vals)) stride firstLayout elemAlign total
          let (cid, st3) = emitSpecConstComposite layout (map (.valId) vals') st2
          Right (st3, Value layout cid)

emitSpecConstScalarCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstScalarCtor ctx constIndex fnIndex structIndex scalar args st =
  case args of
    [] ->
      emitZeroSpecConstScalar scalar st
    [arg] -> do
      (st1, val) <- emitSpecConstExpr ctx constIndex fnIndex structIndex st arg
      case val.valType of
        TLScalar s _ _ | s == scalar -> Right (st1, val)
        TLScalar s _ _ -> do
          (st2, val') <- emitSpecConstScalarConvert s scalar val st1
          Right (st2, val')
        _ -> Left (CompileError "scalar constant cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar constant cast requires a single argument" Nothing Nothing)

coerceSpecConstArgsToLayouts :: [Value] -> [TypeLayout] -> GenState -> Either CompileError (GenState, [Value])
coerceSpecConstArgsToLayouts vals tys st = go st [] vals tys
  where
    go st' acc [] [] = Right (st', reverse acc)
    go st' acc (v:vs) (t:ts) = do
      (st1, v') <- coerceSpecConstValueToLayout t v st'
      go st1 (v':acc) vs ts
    go _ _ _ _ = Left (CompileError "constructor arity mismatch" Nothing Nothing)

emitConstExprList :: GenState -> [Expr] -> Either CompileError (GenState, [Value])
emitConstExprList st = go st []
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (e:es) = do
      (st1, v) <- emitConstExpr st' e
      go st1 (v:acc) es

emitConstVectorCtor :: Int -> Maybe Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstVectorCtor n targetScalar args st =
  if null args
    then
      case targetScalar of
        Nothing -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        Just scalar -> do
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          emitZeroConstValue layout st
    else do
      (st1, vals) <- emitConstExprList st args
      let singleScalar =
            case vals of
              [v] ->
                case v.valType of
                  TLScalar {} -> True
                  _ -> False
              _ -> False
      flattened <- flattenConstScalars st1 vals
      case flattened of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        _ -> do
          let baseLayout = case targetScalar of
                Just s ->
                  let (a, sz) = scalarLayout s
                  in TLScalar s a sz
                Nothing -> pickBaseLayout st1 flattened
          (st2, vals') <- coerceConstValuesToLayout baseLayout flattened st1
          scalar <- case baseLayout of
            TLScalar s _ _ -> Right s
            _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
          let filled =
                case (singleScalar, vals') of
                  (True, [v]) -> replicate n v
                  _ -> vals'
          when (length filled /= n) $
            Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          let (cid, st3) = emitConstComposite layout (map (.valId) filled) st2
          Right (st3, Value layout cid)

emitConstMatrixCtor :: Int -> Int -> Maybe Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstMatrixCtor cols rows targetScalar args st =
  if null args
    then
      case targetScalar of
        Nothing -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        Just scalar ->
          emitZeroConstValue (matrixLayout cols rows scalar) st
    else do
      (st1, vals) <- emitConstExprList st args
      case vals of
        [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        [v] | isScalarLayout v.valType -> do
          (st2, scalarVal, scalar) <- coerceSingleScalar v st1
          (st3, zeroVal) <- emitConstZeroScalar scalar st2
          let (va, vsz) = vectorLayout scalar rows
          let vecLayout = TLVector rows scalar va vsz
          (st4, colsVals) <- buildDiagColumns vecLayout scalarVal zeroVal st3
          let layout = matrixLayout cols rows scalar
          let (cid, st5) = emitConstComposite layout (map (.valId) colsVals) st4
          Right (st5, Value layout cid)
        _ -> do
          let scalarCount = cols * rows
          flattened <- flattenMatrixArgs rows vals st1
          case flattened of
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
            _ -> do
              let baseLayout = case targetScalar of
                    Just scalar ->
                      let (a, sz) = scalarLayout scalar
                      in TLScalar scalar a sz
                    Nothing -> pickBaseLayout st1 flattened
              (st2, vals') <- coerceConstValuesToLayout baseLayout flattened st1
              scalar <- case baseLayout of
                TLScalar s _ _ -> Right s
                _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
              when (length vals' /= scalarCount) $
                Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
              let (va, vsz) = vectorLayout scalar rows
              let vecLayout = TLVector rows scalar va vsz
              (st3, colsVals) <- buildColumns vecLayout vals' st2
              let layout = matrixLayout cols rows scalar
              let (cid, st4) = emitConstComposite layout (map (.valId) colsVals) st3
              Right (st4, Value layout cid)
  where
    isScalarLayout layout =
      case layout of
        TLScalar {} -> True
        _ -> False
    coerceSingleScalar v st' = do
      case targetScalar of
        Just s -> do
          let (a, sz) = scalarLayout s
          (st1, vals') <- coerceConstValuesToLayout (TLScalar s a sz) [v] st'
          case vals' of
            [v'] -> Right (st1, v', s)
            _ -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        Nothing ->
          case v.valType of
            TLScalar s _ _ -> Right (st', v, s)
            _ -> Left (CompileError "matrix constructor expects a scalar" Nothing Nothing)
    emitConstZeroScalar scalar st' =
      case scalar of
        F16 -> do
          let (cid, st1) = emitConstF16 st' 0
          let (a, sz) = scalarLayout F16
          Right (st1, Value (TLScalar F16 a sz) cid)
        F32 -> do
          let (cid, st1) = emitConstF32 st' 0
          let (a, sz) = scalarLayout F32
          Right (st1, Value (TLScalar F32 a sz) cid)
        I32 -> do
          (cid, st1) <- emitConstIntScalar I32 0 st'
          let (a, sz) = scalarLayout I32
          Right (st1, Value (TLScalar I32 a sz) cid)
        U32 -> do
          (cid, st1) <- emitConstIntScalar U32 0 st'
          let (a, sz) = scalarLayout U32
          Right (st1, Value (TLScalar U32 a sz) cid)
        Bool -> do
          let (cid, st1) = emitConstBool st' False
          let (a, sz) = scalarLayout Bool
          Right (st1, Value (TLScalar Bool a sz) cid)
    flattenMatrixArgs rows' vals st' = fmap concat (mapM (flattenOne st') vals)
      where
        flattenOne st'' v =
          case v.valType of
            TLScalar {} -> Right [v]
            TLVector n _ _ _
              | n == rows' -> flattenConstScalars st'' [v]
              | otherwise ->
                  Left (CompileError "matrix constructor expects column vectors matching the row count" Nothing Nothing)
            _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
    buildDiagColumns vecLayout diagVal zeroVal st' =
      go 0 st'
      where
        go colIx st1
          | colIx >= cols = Right (st1, [])
          | otherwise = do
              let elems =
                    [ if rowIx == colIx then diagVal else zeroVal
                    | rowIx <- [0 .. rows - 1]
                    ]
              let (cid, st2) = emitConstComposite vecLayout (map (.valId) elems) st1
              (st3, rest) <- go (colIx + 1) st2
              Right (st3, Value vecLayout cid : rest)
    buildColumns vecLayout vals st' =
      case splitAt rows vals of
        ([], _) -> Right (st', [])
        (col, rest) -> do
          let (cid, st1) = emitConstComposite vecLayout (map (.valId) col) st'
          (st2, colsVals) <- buildColumns vecLayout rest st1
          Right (st2, Value vecLayout cid : colsVals)

emitConstStructCtor :: Text -> TypeLayout -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstStructCtor name layout args st =
  case layout of
    TLStruct _ fields _ _ -> do
      case args of
        [] -> emitZeroConstValue layout st
        _ -> do
          when (length args /= length fields) $
            Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
          (st1, vals) <- emitConstExprList st args
          (st2, vals') <- coerceConstArgsToLayouts vals (map (.flType) fields) st1
          let (cid, st3) = emitConstComposite layout (map (.valId) vals') st2
          Right (st3, Value layout cid)
    _ -> Left (CompileError ("unsupported constructor: " <> textToString name) Nothing Nothing)

emitConstArrayCtor :: [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstArrayCtor args st =
  case args of
    [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
    _ -> do
      (st1, vals) <- emitConstExprList st args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        _ -> do
          let firstLayout = pickBaseLayout st1 vals
          (st2, vals') <- coerceConstValuesToLayout firstLayout vals st1
          when (containsResource firstLayout) $
            Left (CompileError "arrays of resources are not supported" Nothing Nothing)
          when (containsAtomic firstLayout) $
            Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
          let elemAlign = layoutAlign firstLayout
          let elemSize = layoutSize firstLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral (length vals')
          let layout = TLArray (Just (length vals)) stride firstLayout elemAlign total
          let (cid, st3) = emitConstComposite layout (map (.valId) vals') st2
          Right (st3, Value layout cid)

emitConstScalarCtor :: Scalar -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstScalarCtor scalar args st =
  case args of
    [] ->
      emitZeroConstScalar scalar st
    [arg] -> do
      (st1, val) <- emitConstExpr st arg
      case val.valType of
        TLScalar s _ _ | s == scalar -> Right (st1, val)
        TLScalar {} ->
          case lookupConstKeyById st1 (val.valId) of
            Nothing -> Left (CompileError "scalar constant cast requires a literal value" Nothing Nothing)
            Just key -> do
              key' <- convertConstKey key scalar
              let (cid, st2) = emitConstFromKey st1 key'
              let (a, sz) = scalarLayout scalar
              let layout = TLScalar scalar a sz
              Right (st2, Value layout cid)
        _ -> Left (CompileError "scalar constant cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar constant cast requires a single argument" Nothing Nothing)

lookupConstKeyById :: GenState -> Word32 -> Maybe ConstKey
lookupConstKeyById st cid = Map.lookup cid st.gsConstKeyById

emitConstFromKey :: GenState -> ConstKey -> (Word32, GenState)
emitConstFromKey st key =
  case key of
    ConstU32 v -> emitConstU32 st v
    ConstI32 v -> emitConstI32 st v
    ConstF32 bits -> emitConstF32Bits st bits
    ConstF16 bits -> emitConstF16Bits st bits
    ConstBool b -> emitConstBool st b

convertConstKey :: ConstKey -> Scalar -> Either CompileError ConstKey
convertConstKey key target =
  case target of
    U32 -> ConstU32 <$> constKeyToU32 key
    I32 -> ConstI32 . fromIntegral <$> constKeyToI32 key
    F32 -> ConstF32 . castFloatToWord32 <$> constKeyToFloat key
    F16 -> ConstF16 . floatToHalfBits <$> constKeyToFloat key
    Bool ->
      case key of
        ConstBool b -> Right (ConstBool b)
        _ -> Left (CompileError "bool constant cast requires a bool literal" Nothing Nothing)

constKeyToFloat :: ConstKey -> Either CompileError Float
constKeyToFloat key =
  case key of
    ConstF32 bits -> Right (castWord32ToFloat bits)
    ConstF16 bits -> Right (halfBitsToFloat bits)
    ConstU32 v -> Right (fromIntegral v)
    ConstI32 v -> Right (fromIntegral (fromIntegral v :: Int32))
    ConstBool _ -> Left (CompileError "cannot convert bool literal to float" Nothing Nothing)

constKeyToI32 :: ConstKey -> Either CompileError Int32
constKeyToI32 key =
  case key of
    ConstI32 v -> Right (fromIntegral v)
    ConstU32 v -> Right (fromIntegral v)
    ConstF32 bits -> Right (truncate (castWord32ToFloat bits))
    ConstF16 bits -> Right (truncate (halfBitsToFloat bits))
    ConstBool _ -> Left (CompileError "cannot convert bool literal to i32" Nothing Nothing)

constKeyToU32 :: ConstKey -> Either CompileError Word32
constKeyToU32 key =
  case key of
    ConstU32 v -> Right v
    ConstI32 v -> Right (fromIntegral (fromIntegral v :: Int32))
    ConstF32 bits -> Right (fromIntegral (truncate (castWord32ToFloat bits) :: Integer))
    ConstF16 bits -> Right (fromIntegral (truncate (halfBitsToFloat bits) :: Integer))
    ConstBool _ -> Left (CompileError "cannot convert bool literal to u32" Nothing Nothing)

-- SPIR-V builder types

data Instr = Instr Word16 [Word32] deriving (Eq, Show)

encodeInstr :: Instr -> [Word32]
encodeInstr (Instr opcode ops) =
  let wc = 1 + length ops
      first = (fromIntegral wc `shiftL` 16) .|. fromIntegral opcode
  in first : ops

-- Minimal subset of opcodes and enums

opCapability :: Word16
opCapability = 17

opMemoryModel :: Word16
opMemoryModel = 14

opEntryPoint :: Word16
opEntryPoint = 15

opExtInstImport :: Word16
opExtInstImport = 11

opExtInst :: Word16
opExtInst = 12

opExecutionMode :: Word16
opExecutionMode = 16

opName :: Word16
opName = 5

opMemberName :: Word16
opMemberName = 6

opDecorate :: Word16
opDecorate = 71

opMemberDecorate :: Word16
opMemberDecorate = 72

opTypeVoid :: Word16
opTypeVoid = 19

opTypeBool :: Word16
opTypeBool = 20

opTypeInt :: Word16
opTypeInt = 21

opTypeFloat :: Word16
opTypeFloat = 22

opTypeVector :: Word16
opTypeVector = 23

opTypeMatrix :: Word16
opTypeMatrix = 24

opTypeImage :: Word16
opTypeImage = 25

opTypeSampler :: Word16
opTypeSampler = 26

opTypeSampledImage :: Word16
opTypeSampledImage = 27

opTypeArray :: Word16
opTypeArray = 28

opTypeRuntimeArray :: Word16
opTypeRuntimeArray = 29

opTypeStruct :: Word16
opTypeStruct = 30

opTypePointer :: Word16
opTypePointer = 32

opTypeFunction :: Word16
opTypeFunction = 33

opConstantTrue :: Word16
opConstantTrue = 41

opConstantFalse :: Word16
opConstantFalse = 42

opConstant :: Word16
opConstant = 43

opConstantComposite :: Word16
opConstantComposite = 44

opSpecConstantTrue :: Word16
opSpecConstantTrue = 48

opSpecConstantFalse :: Word16
opSpecConstantFalse = 49

opSpecConstant :: Word16
opSpecConstant = 50

opSpecConstantComposite :: Word16
opSpecConstantComposite = 51

opSpecConstantOp :: Word16
opSpecConstantOp = 52


opLoad :: Word16
opLoad = 61

opStore :: Word16
opStore = 62

opAccessChain :: Word16
opAccessChain = 65

opArrayLength :: Word16
opArrayLength = 68

opVariable :: Word16
opVariable = 59

opFunction :: Word16
opFunction = 54

opFunctionParameter :: Word16
opFunctionParameter = 55

opLabel :: Word16
opLabel = 248

opReturn :: Word16
opReturn = 253

opReturnValue :: Word16
opReturnValue = 254

opKill :: Word16
opKill = 252

opFunctionCall :: Word16
opFunctionCall = 57

opFunctionEnd :: Word16
opFunctionEnd = 56

opUnreachable :: Word16
opUnreachable = 255

opCompositeConstruct :: Word16
opCompositeConstruct = 80

opVectorShuffle :: Word16
opVectorShuffle = 79

opCompositeExtract :: Word16
opCompositeExtract = 81

opTranspose :: Word16
opTranspose = 84

opSampledImage :: Word16
opSampledImage = 86

opImageSampleImplicitLod :: Word16
opImageSampleImplicitLod = 87

opImageSampleExplicitLod :: Word16
opImageSampleExplicitLod = 88

opImageSampleDrefImplicitLod :: Word16
opImageSampleDrefImplicitLod = 89

opImageSampleDrefExplicitLod :: Word16
opImageSampleDrefExplicitLod = 90

opImageFetch :: Word16
opImageFetch = 95

opImageGather :: Word16
opImageGather = 96

opImageDrefGather :: Word16
opImageDrefGather = 97

opImageRead :: Word16
opImageRead = 98

opImageWrite :: Word16
opImageWrite = 99

opImage :: Word16
opImage = 100

opImageQuerySizeLod :: Word16
opImageQuerySizeLod = 103

opImageQuerySize :: Word16
opImageQuerySize = 104

opImageQueryLevels :: Word16
opImageQueryLevels = 106

opImageQuerySamples :: Word16
opImageQuerySamples = 107

opSNegate :: Word16
opSNegate = 126

opFNegate :: Word16
opFNegate = 127

opConvertFToU :: Word16
opConvertFToU = 109

opConvertFToS :: Word16
opConvertFToS = 110

opConvertSToF :: Word16
opConvertSToF = 111

opConvertUToF :: Word16
opConvertUToF = 112

opFConvert :: Word16
opFConvert = 115

opBitcast :: Word16
opBitcast = 124

opIAdd :: Word16
opIAdd = 128

opFAdd :: Word16
opFAdd = 129

opISub :: Word16
opISub = 130

opFSub :: Word16
opFSub = 131

opIMul :: Word16
opIMul = 132

opFMul :: Word16
opFMul = 133

opUDiv :: Word16
opUDiv = 134

opSDiv :: Word16
opSDiv = 135

opFDiv :: Word16
opFDiv = 136

opUMod :: Word16
opUMod = 137

opSRem :: Word16
opSRem = 138

opVectorTimesMatrix :: Word16
opVectorTimesMatrix = 144

opMatrixTimesVector :: Word16
opMatrixTimesVector = 145

opMatrixTimesMatrix :: Word16
opMatrixTimesMatrix = 146

opDot :: Word16
opDot = 148

opAny :: Word16
opAny = 154

opAll :: Word16
opAll = 155

opLogicalEqual :: Word16
opLogicalEqual = 164

opLogicalNotEqual :: Word16
opLogicalNotEqual = 165

opLogicalOr :: Word16
opLogicalOr = 166

opLogicalAnd :: Word16
opLogicalAnd = 167

opLogicalNot :: Word16
opLogicalNot = 168

opSelect :: Word16
opSelect = 169

opIEqual :: Word16
opIEqual = 170

opINotEqual :: Word16
opINotEqual = 171

opUGreaterThan :: Word16
opUGreaterThan = 172

opSGreaterThan :: Word16
opSGreaterThan = 173

opUGreaterThanEqual :: Word16
opUGreaterThanEqual = 174

opSGreaterThanEqual :: Word16
opSGreaterThanEqual = 175

opULessThan :: Word16
opULessThan = 176

opSLessThan :: Word16
opSLessThan = 177

opULessThanEqual :: Word16
opULessThanEqual = 178

opSLessThanEqual :: Word16
opSLessThanEqual = 179

opFOrdEqual :: Word16
opFOrdEqual = 180

opFOrdNotEqual :: Word16
opFOrdNotEqual = 182

opFOrdLessThan :: Word16
opFOrdLessThan = 184

opFOrdGreaterThan :: Word16
opFOrdGreaterThan = 186

opFOrdLessThanEqual :: Word16
opFOrdLessThanEqual = 188

opFOrdGreaterThanEqual :: Word16
opFOrdGreaterThanEqual = 190

opShiftRightLogical :: Word16
opShiftRightLogical = 194

opShiftRightArithmetic :: Word16
opShiftRightArithmetic = 195

opShiftLeftLogical :: Word16
opShiftLeftLogical = 196

opBitwiseOr :: Word16
opBitwiseOr = 197

opBitwiseXor :: Word16
opBitwiseXor = 198

opBitwiseAnd :: Word16
opBitwiseAnd = 199

opBitFieldInsert :: Word16
opBitFieldInsert = 201

opBitFieldSExtract :: Word16
opBitFieldSExtract = 202

opBitFieldUExtract :: Word16
opBitFieldUExtract = 203

opBitReverse :: Word16
opBitReverse = 204

opBitCount :: Word16
opBitCount = 205

opDPdx :: Word16
opDPdx = 207

opDPdy :: Word16
opDPdy = 208

opFwidth :: Word16
opFwidth = 209

opAtomicLoad :: Word16
opAtomicLoad = 227

opAtomicStore :: Word16
opAtomicStore = 228

opAtomicIAdd :: Word16
opAtomicIAdd = 234

opAtomicISub :: Word16
opAtomicISub = 235

opAtomicSMin :: Word16
opAtomicSMin = 236

opAtomicUMin :: Word16
opAtomicUMin = 237

opAtomicSMax :: Word16
opAtomicSMax = 238

opAtomicUMax :: Word16
opAtomicUMax = 239

opAtomicAnd :: Word16
opAtomicAnd = 240

opAtomicOr :: Word16
opAtomicOr = 241

opAtomicXor :: Word16
opAtomicXor = 242

opAtomicExchange :: Word16
opAtomicExchange = 229

opAtomicCompareExchange :: Word16
opAtomicCompareExchange = 230

opControlBarrier :: Word16
opControlBarrier = 224

opLoopMerge :: Word16
opLoopMerge = 246

opSelectionMerge :: Word16
opSelectionMerge = 247

opBranch :: Word16
opBranch = 249

opBranchConditional :: Word16
opBranchConditional = 250

capabilityShader :: Word32
capabilityShader = 1

capabilityFloat16 :: Word32
capabilityFloat16 = 9

capabilitySampled1D :: Word32
capabilitySampled1D = 43

capabilityImage1D :: Word32
capabilityImage1D = 44

capabilityImageQuery :: Word32
capabilityImageQuery = 50

addressingLogical :: Word32
addressingLogical = 0

memoryModelGLSL450 :: Word32
memoryModelGLSL450 = 1

memoryScopeDevice :: Word32
memoryScopeDevice = 1

memoryScopeWorkgroup :: Word32
memoryScopeWorkgroup = 2

memorySemanticsRelaxed :: Word32
memorySemanticsRelaxed = 0

memorySemanticsAcquireRelease :: Word32
memorySemanticsAcquireRelease = 0x8

memorySemanticsUniformMemory :: Word32
memorySemanticsUniformMemory = 0x40

memorySemanticsWorkgroupMemory :: Word32
memorySemanticsWorkgroupMemory = 0x100

memorySemanticsImageMemory :: Word32
memorySemanticsImageMemory = 0x800

executionModelGLCompute :: Word32
executionModelGLCompute = 5

executionModelFragment :: Word32
executionModelFragment = 4

executionModelVertex :: Word32
executionModelVertex = 0

executionModeLocalSize :: Word32
executionModeLocalSize = 17

executionModeOriginUpperLeft :: Word32
executionModeOriginUpperLeft = 7

storageClassInput :: Word32
storageClassInput = 1

storageClassUniformConstant :: Word32
storageClassUniformConstant = 0

storageClassUniform :: Word32
storageClassUniform = 2

storageClassOutput :: Word32
storageClassOutput = 3

storageClassStorageBuffer :: Word32
storageClassStorageBuffer = 12

storageClassFunction :: Word32
storageClassFunction = 7

storageClassWorkgroup :: Word32
storageClassWorkgroup = 4

storageClassPrivate :: Word32
storageClassPrivate = 6

decorationBlock :: Word32
decorationBlock = 2

decorationSpecId :: Word32
decorationSpecId = 1

decorationArrayStride :: Word32
decorationArrayStride = 6

decorationDescriptorSet :: Word32
decorationDescriptorSet = 34

decorationBinding :: Word32
decorationBinding = 33

decorationOffset :: Word32
decorationOffset = 35

decorationBuiltIn :: Word32
decorationBuiltIn = 11

decorationNoPerspective :: Word32
decorationNoPerspective = 13

decorationFlat :: Word32
decorationFlat = 14

decorationCentroid :: Word32
decorationCentroid = 16

decorationSample :: Word32
decorationSample = 17

decorationInvariant :: Word32
decorationInvariant = 18

decorationLocation :: Word32
decorationLocation = 30

decorationIndex :: Word32
decorationIndex = 32

dim1D :: Word32
dim1D = 0

dim2D :: Word32
dim2D = 1

dim3D :: Word32
dim3D = 2

dimCube :: Word32
dimCube = 3

imageSampled :: Word32
imageSampled = 1

imageStorage :: Word32
imageStorage = 2

imageOperandsBias :: Word32
imageOperandsBias = 1

imageOperandsLod :: Word32
imageOperandsLod = 2

imageOperandsGrad :: Word32
imageOperandsGrad = 4

imageOperandsSample :: Word32
imageOperandsSample = 64


functionControlNone :: Word32
functionControlNone = 0

selectionControlNone :: Word32
selectionControlNone = 0

loopControlNone :: Word32
loopControlNone = 0

glslStd450FAbs :: Word32
glslStd450FAbs = 4

glslStd450Round :: Word32
glslStd450Round = 1

glslStd450RoundEven :: Word32
glslStd450RoundEven = 2

glslStd450Trunc :: Word32
glslStd450Trunc = 3

glslStd450Floor :: Word32
glslStd450Floor = 8

glslStd450Ceil :: Word32
glslStd450Ceil = 9

glslStd450Fract :: Word32
glslStd450Fract = 10

glslStd450Radians :: Word32
glslStd450Radians = 11

glslStd450Degrees :: Word32
glslStd450Degrees = 12

glslStd450Sin :: Word32
glslStd450Sin = 13

glslStd450Cos :: Word32
glslStd450Cos = 14

glslStd450Tan :: Word32
glslStd450Tan = 15

glslStd450Asin :: Word32
glslStd450Asin = 16

glslStd450Acos :: Word32
glslStd450Acos = 17

glslStd450Atan :: Word32
glslStd450Atan = 18

glslStd450Sinh :: Word32
glslStd450Sinh = 19

glslStd450Cosh :: Word32
glslStd450Cosh = 20

glslStd450Tanh :: Word32
glslStd450Tanh = 21

glslStd450Asinh :: Word32
glslStd450Asinh = 22

glslStd450Acosh :: Word32
glslStd450Acosh = 23

glslStd450Atanh :: Word32
glslStd450Atanh = 24

glslStd450Atan2 :: Word32
glslStd450Atan2 = 25

glslStd450Pow :: Word32
glslStd450Pow = 26

glslStd450Exp :: Word32
glslStd450Exp = 27

glslStd450Log :: Word32
glslStd450Log = 28

glslStd450Exp2 :: Word32
glslStd450Exp2 = 29

glslStd450Log2 :: Word32
glslStd450Log2 = 30

glslStd450Sqrt :: Word32
glslStd450Sqrt = 31

glslStd450InverseSqrt :: Word32
glslStd450InverseSqrt = 32

glslStd450Determinant :: Word32
glslStd450Determinant = 33

glslStd450MatrixInverse :: Word32
glslStd450MatrixInverse = 34

glslStd450ModfStruct :: Word32
glslStd450ModfStruct = 36

glslStd450FMin :: Word32
glslStd450FMin = 37

glslStd450FMax :: Word32
glslStd450FMax = 40

glslStd450FClamp :: Word32
glslStd450FClamp = 43

glslStd450FMix :: Word32
glslStd450FMix = 46

glslStd450Step :: Word32
glslStd450Step = 48

glslStd450SmoothStep :: Word32
glslStd450SmoothStep = 49

glslStd450Fma :: Word32
glslStd450Fma = 50

glslStd450FrexpStruct :: Word32
glslStd450FrexpStruct = 52

glslStd450Ldexp :: Word32
glslStd450Ldexp = 53

glslStd450PackSnorm4x8 :: Word32
glslStd450PackSnorm4x8 = 54

glslStd450PackUnorm4x8 :: Word32
glslStd450PackUnorm4x8 = 55

glslStd450PackSnorm2x16 :: Word32
glslStd450PackSnorm2x16 = 56

glslStd450PackUnorm2x16 :: Word32
glslStd450PackUnorm2x16 = 57

glslStd450PackHalf2x16 :: Word32
glslStd450PackHalf2x16 = 58

glslStd450UnpackSnorm2x16 :: Word32
glslStd450UnpackSnorm2x16 = 60

glslStd450UnpackUnorm2x16 :: Word32
glslStd450UnpackUnorm2x16 = 61

glslStd450UnpackHalf2x16 :: Word32
glslStd450UnpackHalf2x16 = 62

glslStd450UnpackSnorm4x8 :: Word32
glslStd450UnpackSnorm4x8 = 63

glslStd450UnpackUnorm4x8 :: Word32
glslStd450UnpackUnorm4x8 = 64

glslStd450Length :: Word32
glslStd450Length = 66

glslStd450Cross :: Word32
glslStd450Cross = 68

glslStd450Normalize :: Word32
glslStd450Normalize = 69

glslStd450FaceForward :: Word32
glslStd450FaceForward = 70

glslStd450Refract :: Word32
glslStd450Refract = 72

glslStd450FindILsb :: Word32
glslStd450FindILsb = 73

glslStd450FindSMsb :: Word32
glslStd450FindSMsb = 74

glslStd450FindUMsb :: Word32
glslStd450FindUMsb = 75

-- Generator state

data GenState = GenState
  { gsNextId :: Word32
  , gsStructLayouts :: [(Text, TypeLayout)]
  , gsStructIds :: [(Text, Word32)]
  , gsBlockStructs :: [Text]
  , gsTypeCache :: Map.Map TypeKey Word32
  , gsConstCache :: Map.Map ConstKey Word32
  , gsConstKeyById :: Map.Map Word32 ConstKey
  , gsSpecConstLiteralIds :: Set.Set Word32
  , gsExtInstIds :: [(String, Word32)]
  , gsExtInstImports :: [Instr]
  , gsGlobalVars :: [(Text, VarInfo)]
  , gsFunctionTable :: [FunctionInfo]
  , gsFunctionsByName :: Map.Map Text [FunctionInfo]
  , gsEntryStage :: Stage
  , gsEnabledFeatures :: Set.Set Text
  , gsConstValues :: [(Text, Value)]
  , gsConstValuesByName :: Map.Map Text Value
  , gsConstComposites :: Map.Map Word32 (TypeLayout, [Word32])
  , gsCapabilities :: [Word32]
  , gsNames :: [Instr]
  , gsDecorations :: [Instr]
  , gsTypes :: [Instr]
  , gsConstants :: [Instr]
  , gsGlobals :: [Instr]
  , gsFunctions :: [Instr]
  , gsEntryPoint :: Maybe Word32
  , gsInterfaceIds :: [Word32]
  , gsSamplerMode :: SamplerBindingMode
  , gsSamplerLayouts :: Map.Map Text TypeLayout
  }

emptyGenState :: SamplerBindingMode -> Stage -> [(Text, TypeLayout)] -> [Text] -> Map.Map Text TypeLayout -> [Text] -> GenState
emptyGenState samplerMode stage structLayouts blockStructs samplerLayouts enabledFeatures =
  let (ids, nextId) = assignStructIds 1 structLayouts
  in GenState
      { gsNextId = nextId
      , gsStructLayouts = structLayouts
      , gsStructIds = ids
      , gsBlockStructs = blockStructs
      , gsTypeCache = Map.empty
      , gsConstCache = Map.empty
      , gsConstKeyById = Map.empty
      , gsSpecConstLiteralIds = Set.empty
      , gsExtInstIds = []
      , gsExtInstImports = []
      , gsGlobalVars = []
      , gsFunctionTable = []
      , gsFunctionsByName = Map.empty
      , gsEntryStage = stage
      , gsEnabledFeatures = Set.fromList enabledFeatures
      , gsConstValues = []
      , gsConstValuesByName = Map.empty
      , gsConstComposites = Map.empty
      , gsCapabilities = []
      , gsNames = []
      , gsDecorations = []
      , gsTypes = []
      , gsConstants = []
      , gsGlobals = []
      , gsFunctions = []
      , gsEntryPoint = Nothing
      , gsInterfaceIds = []
      , gsSamplerMode = samplerMode
      , gsSamplerLayouts = samplerLayouts
      }

assignStructIds :: Word32 -> [(Text, TypeLayout)] -> ([(Text, Word32)], Word32)
assignStructIds start layouts =
  let go next acc [] = (reverse acc, next)
      go next acc ((name, _):rest) = go (next + 1) ((name, next):acc) rest
  in go start [] layouts

mapFromAssocFirstWins :: Ord k => [(k, v)] -> Map.Map k v
mapFromAssocFirstWins = foldr (uncurry Map.insert) Map.empty
{-# INLINE mapFromAssocFirstWins #-}

freshId :: GenState -> (Word32, GenState)
freshId st = (st.gsNextId, st { gsNextId = st.gsNextId + 1 })

addInstr :: (GenState -> [Instr]) -> (GenState -> [Instr] -> GenState) -> Instr -> GenState -> GenState
addInstr getter setter instr st =
  let xs = getter st
  in setter st (instr : xs)

addCapability :: Word32 -> GenState -> GenState
addCapability cap st =
  if cap `elem` st.gsCapabilities
    then st
    else st { gsCapabilities = st.gsCapabilities <> [cap] }

addName :: Instr -> GenState -> GenState
addName = addInstr (.gsNames) (\st v -> st { gsNames = v })

addDecoration :: Instr -> GenState -> GenState
addDecoration = addInstr (.gsDecorations) (\st v -> st { gsDecorations = v })

addType :: Instr -> GenState -> GenState
addType = addInstr (.gsTypes) (\st v -> st { gsTypes = v })

addConst :: Instr -> GenState -> GenState
addConst instr st =
  let st1 = addInstr (.gsConstants) (\st' v -> st' { gsConstants = v }) instr st
  in case specConstLiteralResultId instr of
      Nothing -> st1
      Just rid -> st1 { gsSpecConstLiteralIds = Set.insert rid st1.gsSpecConstLiteralIds }

specConstLiteralResultId :: Instr -> Maybe Word32
specConstLiteralResultId (Instr op ops)
  | op == opSpecConstantTrue || op == opSpecConstantFalse =
      case ops of
        (_ty:rid:_) -> Just rid
        _ -> Nothing
  | op == opSpecConstant || op == opSpecConstantComposite =
      case ops of
        (_ty:rid:_) -> Just rid
        _ -> Nothing
  | otherwise = Nothing

addGlobal :: Instr -> GenState -> GenState
addGlobal = addInstr (.gsGlobals) (\st v -> st { gsGlobals = v })

-- Emit struct types with member decorations
emitStructs :: GenState -> ((), GenState)
emitStructs st0 =
  let st1 = foldl' emitStruct st0 st0.gsStructLayouts
  in ((), st1)
  where
    emitStruct st (name, layout) =
      case layout of
        TLStruct _ fields _ _ ->
          let (structId, st1) =
                case lookup name (st.gsStructIds) of
                  Just sid -> (sid, st)
                  Nothing ->
                    let (sid, st') = freshId st
                    in (sid, st' { gsStructIds = (name, sid) : st'.gsStructIds })
              (st2, fieldTypeIds) = mapAccumL emitFieldType st1 fields
              st3 = addType (Instr opTypeStruct (structId : fieldTypeIds)) st2
              st4 = addName (Instr opName (structId : encodeString (textToString name))) st3
              st5 = foldl' (emitMemberDecorate structId) st4 (zip [0 :: Int ..] fields)
              st6 = if name `elem` st5.gsBlockStructs
                then addDecoration (Instr opDecorate [structId, decorationBlock]) st5
                else st5
              st7 = foldl' (emitMemberName structId) st6 (zip [0 :: Int ..] fields)
          in st7
        _ -> st

    emitFieldType st field =
      let (tyId, st') = emitTypeFromLayout st field.flType
      in (st', tyId)

    emitMemberDecorate structId st (ix, field) =
      let offset = field.flOffset
      in addDecoration (Instr opMemberDecorate [structId, fromIntegral ix, decorationOffset, offset]) st

    emitMemberName structId st (ix, field) =
      addName (Instr opMemberName (structId : fromIntegral ix : encodeString field.flName)) st

data VarAccess = ReadOnly | ReadWrite
  deriving (Eq, Show)

ptrAccessToVarAccess :: Maybe StorageAccess -> VarAccess
ptrAccessToVarAccess acc =
  if ptrAccessAllowsWrite acc then ReadWrite else ReadOnly

varAccessToPtrAccess :: VarAccess -> Maybe StorageAccess
varAccessToPtrAccess acc =
  case acc of
    ReadOnly -> Just StorageRead
    ReadWrite -> Just StorageReadWrite

data VarInfo = VarInfo
  { viType :: !TypeLayout
  , viPtrId :: !Word32
  , viStorage :: !Word32
  , viAccess :: !VarAccess
  , viPath :: ![Word32]
  } deriving (Eq, Show)

data EntryFieldInit = EntryFieldInit
  { efiLayout :: !TypeLayout
  , efiVar :: !VarInfo
  } deriving (Eq, Show)

data EntryParamInit = EntryParamInit
  { epiName :: !Text
  , epiLayout :: !TypeLayout
  , epiFields :: ![EntryFieldInit]
  } deriving (Eq, Show)

data OutputTarget = OutputTarget
  { otVar :: !VarInfo
  , otLayout :: !TypeLayout
  , otPath :: ![Word32]
  } deriving (Eq, Show)

data Value = Value
  { valType :: !TypeLayout
  , valId :: !Word32
  } deriving (Eq, Show)

data FunctionInfo = FunctionInfo
  { fiName :: !Text
  , fiParams :: ![TypeLayout]
  , fiReturn :: !(Maybe TypeLayout)
  , fiId :: !Word32
  , fiTypeId :: !Word32
  } deriving (Eq, Show)

data FuncState = FuncState
  { fsLocals :: ![Instr]
  , fsInstrs :: ![Instr]
  , fsVars :: ![(Text, VarInfo)]
  , fsVarsByName :: !(Map.Map Text VarInfo)
  , fsValues :: ![(Text, Value)]
  , fsValuesByName :: !(Map.Map Text Value)
  , fsTerminated :: !Bool
  , fsLoopStack :: ![(Word32, Word32)]
  , fsBreakStack :: ![Word32]
  } deriving (Eq, Show)

emitGlobals :: StructLayoutCache -> [(Text, StructDecl)] -> ShaderInterface -> EntryPoint -> Maybe TypeLayout -> [GlobalVarDecl] -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], [OutputTarget], GenState)
emitGlobals layoutCache structEnv iface entry retLayout globals st0 = do
  let (envBindings, idsBindings, st1) = foldl' emitBinding ([], [], st0) (iface.siBindings)
  (envGlobals, st2) <- emitModuleGlobals layoutCache globals st1
  (envInputs, entryInits, idsInputs, st3) <- emitEntryInputs layoutCache structEnv entry st2
  (outTargets, idsOut, st4) <- emitStageOutput layoutCache structEnv entry retLayout st3
  let envAll = envBindings <> envGlobals <> envInputs
  let idsGlobals = map ((.viPtrId) . snd) envGlobals
  let ifaceIds = idsBindings <> idsInputs <> idsOut <> idsGlobals
  let st5 = st4 { gsInterfaceIds = ifaceIds, gsGlobalVars = envBindings <> envGlobals }
  pure (envAll, entryInits, ifaceIds, outTargets, st5)
  where
    emitBinding (envAcc, idAcc, st) bindInfo =
      let (ptrTy, st1) = emitPointerForBinding st bindInfo.biKind bindInfo.biType
          (varId, st2) = freshId st1
          storageClass = case bindInfo.biKind of
            BUniform -> storageClassUniform
            BStorageRead -> storageClassStorageBuffer
            BStorageReadWrite -> storageClassStorageBuffer
            BSampler -> storageClassUniformConstant
            BSamplerComparison -> storageClassUniformConstant
            BTexture1D -> storageClassUniformConstant
            BTexture1DArray -> storageClassUniformConstant
            BTexture2D -> storageClassUniformConstant
            BTexture2DArray -> storageClassUniformConstant
            BTexture3D -> storageClassUniformConstant
            BTextureCube -> storageClassUniformConstant
            BTextureCubeArray -> storageClassUniformConstant
            BTextureMultisampled2D -> storageClassUniformConstant
            BTextureDepth2D -> storageClassUniformConstant
            BTextureDepth2DArray -> storageClassUniformConstant
            BTextureDepthCube -> storageClassUniformConstant
            BTextureDepthCubeArray -> storageClassUniformConstant
            BTextureDepthMultisampled2D -> storageClassUniformConstant
            BStorageTexture1D -> storageClassUniformConstant
            BStorageTexture2D -> storageClassUniformConstant
            BStorageTexture2DArray -> storageClassUniformConstant
            BStorageTexture3D -> storageClassUniformConstant
          st3 = addGlobal (Instr opVariable [ptrTy, varId, storageClass]) st2
          st4 = addDecoration (Instr opDecorate [varId, decorationDescriptorSet, bindInfo.biGroup]) st3
          st5 = addDecoration (Instr opDecorate [varId, decorationBinding, bindInfo.biBinding]) st4
          st6 = addName (Instr opName (varId : encodeString bindInfo.biName)) st5
          access = case bindInfo.biKind of
            BStorageReadWrite -> ReadWrite
            _ -> ReadOnly
          info = VarInfo bindInfo.biType varId storageClass access []
      in (envAcc <> [(T.pack bindInfo.biName, info)], idAcc <> [varId], st6)

emitModuleGlobals :: StructLayoutCache -> [GlobalVarDecl] -> GenState -> Either CompileError ([(Text, VarInfo)], GenState)
emitModuleGlobals layoutCache decls st0 = foldM emitOne ([], st0) decls
  where
    emitOne (envAcc, st) decl = do
      layout <- resolveTypeLayoutWithCache layoutCache (decl.gvType)
      when (containsResource layout) $
        Left (CompileError "global variables cannot contain resource types" Nothing Nothing)
      when (containsAtomic layout) $
        Left (CompileError "atomic types are only supported in storage buffers for now" Nothing Nothing)
      when (containsRuntimeArray layout) $
        Left (CompileError "runtime arrays are only supported in storage buffers" Nothing Nothing)
      storageClass <- case decl.gvSpace of
        "private" -> Right storageClassPrivate
        "workgroup" -> Right storageClassWorkgroup
        other -> Left (CompileError ("unsupported global address space: " <> textToString other) Nothing Nothing)
      (initId, st1) <- case decl.gvInit of
        Nothing -> Right (Nothing, st)
        Just expr -> do
          when (storageClass == storageClassWorkgroup) $
            Left (CompileError "workgroup variables cannot have initializers" Nothing Nothing)
          (st2, val) <- emitConstExpr st expr
          (st3, val') <- coerceConstValueToLayout layout val st2
          Right (Just (val'.valId), st3)
      let (baseTy, st2) = emitTypeFromLayout st1 layout
      let (ptrTy, st3) = emitPointerType st2 storageClass baseTy
      let (varId, st4) = freshId st3
      let operands =
            case initId of
              Nothing -> [ptrTy, varId, storageClass]
              Just cid -> [ptrTy, varId, storageClass, cid]
      let st5 = addGlobal (Instr opVariable operands) st4
      let st6 = addName (Instr opName (varId : encodeString (textToString (decl.gvName)))) st5
      let info = VarInfo layout varId storageClass ReadWrite []
      Right (envAcc <> [(decl.gvName, info)], st6)

emitEntryInputs :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitEntryInputs layoutCache structEnv entry st0 =
  case entry.epStage of
    StageCompute -> emitComputeInputs layoutCache structEnv entry st0
    StageFragment -> emitFragmentInputs layoutCache structEnv entry st0
    StageVertex -> emitVertexInputs layoutCache structEnv entry st0

emitComputeInputs :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitComputeInputs layoutCache =
  emitStageInputs layoutCache StageCompute

emitFragmentInputs :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitFragmentInputs layoutCache structEnv entry st0 = do
  emitStageInputs layoutCache StageFragment structEnv entry st0

emitVertexInputs :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitVertexInputs layoutCache =
  emitStageInputs layoutCache StageVertex

data InputDecoration = InputBuiltin Word32 | InputLocation Word32

emitStageInputs :: StructLayoutCache -> Stage -> [(Text, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(Text, VarInfo)], [EntryParamInit], [Word32], GenState)
emitStageInputs layoutCache stage structEnv entry st0 = do
  let params = entry.epParams
  let go envAcc initAcc idAcc _ _ st [] =
        Right (reverse envAcc, reverse initAcc, reverse idAcc, st)
      go envAcc initAcc idAcc usedLocs usedBuiltins st (param:rest) = do
        (envNext, initNext, idNext, usedLocsNext, usedBuiltinsNext, stNext) <- emitParam stage structEnv param (envAcc, initAcc, idAcc, usedLocs, usedBuiltins, st)
        go envNext initNext idNext usedLocsNext usedBuiltinsNext stNext rest
  go [] [] [] [] [] st0 params
  where
    emitParam stg env (Param _ name ty attrs) (envAcc, initAcc, idAcc, usedLocs, usedBuiltins, st) =
      case ty of
        TyStructRef structName -> do
          when (isJust (paramBuiltin attrs) || isJust (paramLocation attrs)) $
            Left (CompileError "struct parameters cannot have @location or @builtin attributes" Nothing Nothing)
          structDecl <- case lookup structName env of
            Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
            Just decl -> Right decl
          structLayout <- resolveTypeLayoutWithCache layoutCache ty
          when (containsResource structLayout) $
            Left (CompileError "resource types are not allowed as stage inputs" Nothing Nothing)
          when (containsAtomic structLayout) $
            Left (CompileError "atomic types are not allowed as stage inputs" Nothing Nothing)
          let fields = structDecl.sdFields
          (fieldVars, usedLocs', usedBuiltins', ids', st') <- emitStructFields stg name fields usedLocs usedBuiltins idAcc st
          let initEntry = EntryParamInit name structLayout fieldVars
          pure (envAcc, initEntry:initAcc, ids', usedLocs', usedBuiltins', st')
        _ -> do
          layout <- resolveTypeLayoutWithCache layoutCache ty
          when (containsResource layout) $
            Left (CompileError "resource types are not allowed as stage inputs" Nothing Nothing)
          when (containsAtomic layout) $
            Left (CompileError "atomic types are not allowed as stage inputs" Nothing Nothing)
          when (isJust (paramBuiltin attrs) && isJust (paramLocation attrs)) $
            Left (CompileError "parameters cannot have both @location and @builtin" Nothing Nothing)
          case paramBuiltin attrs of
            Just builtin -> do
              when (builtin `elem` usedBuiltins) $
                Left (CompileError ("duplicate @builtin(" <> textToString builtin <> ") on stage inputs") Nothing Nothing)
              expected <- case builtinInputType stg builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                Just ty' -> Right ty'
              when (expected /= ty) $
                Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
              builtinId <- case builtinInputDecoration stg builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                Just bid -> Right bid
              (info, varId, st1) <- emitInputVar stg name layout attrs (InputBuiltin builtinId) st
              pure ((name, info):envAcc, initAcc, varId:idAcc, usedLocs, builtin:usedBuiltins, st1)
            Nothing -> do
              loc <- case paramLocation attrs of
                Nothing ->
                  case stg of
                    StageCompute -> Left (CompileError "compute parameters must use @builtin" Nothing Nothing)
                    _ -> Left (CompileError "stage parameters must use @location or @builtin" Nothing Nothing)
                Just n -> Right n
              when (stg == StageCompute) $
                Left (CompileError "compute parameters cannot use @location" Nothing Nothing)
              when (loc `elem` usedLocs) $
                Left (CompileError "duplicate @location on stage inputs" Nothing Nothing)
              (info, varId, st1) <- emitInputVar stg name layout attrs (InputLocation loc) st
              pure ((name, info):envAcc, initAcc, varId:idAcc, loc:usedLocs, usedBuiltins, st1)

    emitStructFields stg paramName fields usedLocs usedBuiltins idAcc st = do
      let go accVars accIds accLocs accBuiltins st' [] = Right (reverse accVars, accLocs, accBuiltins, accIds, st')
          go accVars accIds accLocs accBuiltins st' (field:rest) = do
            let fieldName = field.fdName
                fullName = paramName <> "_" <> fieldName
                attrs = field.fdAttrs
                fty = field.fdType
            layout <- resolveTypeLayoutWithCache layoutCache fty
            when (containsResource layout) $
              Left (CompileError "resource types are not allowed as stage inputs" Nothing Nothing)
            when (containsAtomic layout) $
              Left (CompileError "atomic types are not allowed as stage inputs" Nothing Nothing)
            when (isJust (attrBuiltin attrs) && isJust (attrLocation attrs)) $
              Left (CompileError "struct fields cannot have both @location and @builtin" Nothing Nothing)
            case attrBuiltin attrs of
              Just builtin -> do
                when (builtin `elem` accBuiltins) $
                  Left (CompileError ("duplicate @builtin(" <> textToString builtin <> ") on stage inputs") Nothing Nothing)
                expected <- case builtinInputType stg builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                  Just ty' -> Right ty'
                when (expected /= fty) $
                  Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
                builtinId <- case builtinInputDecoration stg builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage input") Nothing Nothing)
                  Just bid -> Right bid
                (info, varId, st1) <- emitInputVar stg fullName layout attrs (InputBuiltin builtinId) st'
                let fieldInit = EntryFieldInit layout info
                go (fieldInit:accVars) (varId:accIds) accLocs (builtin:accBuiltins) st1 rest
              Nothing -> do
                loc <- case attrLocation attrs of
                  Nothing ->
                    case stg of
                      StageCompute -> Left (CompileError "compute parameters must use @builtin" Nothing Nothing)
                      _ -> Left (CompileError "struct fields must use @location or @builtin" Nothing Nothing)
                  Just n -> Right n
                when (stg == StageCompute) $
                  Left (CompileError "compute parameters cannot use @location" Nothing Nothing)
                when (loc `elem` accLocs) $
                  Left (CompileError "duplicate @location on stage inputs" Nothing Nothing)
                (info, varId, st1) <- emitInputVar stg fullName layout attrs (InputLocation loc) st'
                let fieldInit = EntryFieldInit layout info
                go (fieldInit:accVars) (varId:accIds) (loc:accLocs) accBuiltins st1 rest
      (vars, locs, builtins, ids, st1) <- go [] idAcc usedLocs usedBuiltins st fields
      pure (vars, locs, builtins, ids, st1)

emitInputVar :: Stage -> Text -> TypeLayout -> [Attr] -> InputDecoration -> GenState -> Either CompileError (VarInfo, Word32, GenState)
emitInputVar stage name layout attrs deco st0 = do
  validateUserStageIOType deco layout
  let (storageLayout, path) = sampleMaskLayout layout deco
  let (baseTy, st1) = emitTypeFromLayout st0 storageLayout
  let (ptrTy, st2) = emitPointerType st1 storageClassInput baseTy
  let (varId, st3) = freshId st2
  let st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassInput]) st3
  let st5a = case deco of
        InputBuiltin bid -> addDecoration (Instr opDecorate [varId, decorationBuiltIn, bid]) st4
        InputLocation loc -> addDecoration (Instr opDecorate [varId, decorationLocation, loc]) st4
  st5 <- applyIOAttrDecorations stage False layout attrs deco varId st5a
  let st6 = addName (Instr opName (varId : encodeString (textToString name))) st5
  let info = VarInfo layout varId storageClassInput ReadOnly path
  pure (info, varId, st6)

emitStageOutput :: StructLayoutCache -> [(Text, StructDecl)] -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitStageOutput _ structEnv entry retLayout st0 =
  case entry.epStage of
    StageCompute ->
      case retLayout of
        Nothing -> Right ([], [], st0)
        Just _ -> Left (CompileError "compute entry points must return void" Nothing Nothing)
    StageFragment ->
      emitFragmentOutput structEnv entry retLayout st0
    StageVertex ->
      emitVertexOutput structEnv entry retLayout st0

emitFragmentOutput :: [(Text, StructDecl)] -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitFragmentOutput structEnv entry retLayout st0 =
  case (retLayout, entry.epReturnType) of
    (Nothing, _) -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)
    (Just layout, Just ty) ->
      case ty of
        TyStructRef structName -> do
          when (isJust entry.epReturnBuiltin || isJust entry.epReturnLocation || not (null entry.epReturnAttrs)) $
            Left (CompileError "struct return values cannot use @location or @builtin on the function" Nothing Nothing)
          structDecl <- case lookup structName structEnv of
            Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
            Just decl -> Right decl
          emitStructOutputs StageFragment structName layout structDecl st0
        _ -> do
          when (containsResource layout) $
            Left (CompileError "resource types are not allowed as fragment outputs" Nothing Nothing)
          when (containsAtomic layout) $
            Left (CompileError "atomic types are not allowed as fragment outputs" Nothing Nothing)
          blendSrc <- parseBlendSrcAttr entry.epReturnAttrs
          when (isJust blendSrc) $
            Left (CompileError "if @blend_src is used, fragment outputs must be exactly two @location(0) fields with @blend_src(0) and @blend_src(1)" Nothing Nothing)
          when (isJust entry.epReturnBuiltin && isJust entry.epReturnLocation) $
            Left (CompileError "fragment returns cannot use both @location and @builtin" Nothing Nothing)
          case entry.epReturnBuiltin of
            Just builtin -> do
              expected <- case builtinOutputType StageFragment builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for fragment output") Nothing Nothing)
                Just t -> Right t
              when (expected /= ty) $
                Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
              builtinId <- case builtinOutputDecoration StageFragment builtin of
                Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for fragment output") Nothing Nothing)
                Just bid -> Right bid
              (info, varId, st1) <- emitOutputVar StageFragment "frag_output" layout entry.epReturnAttrs (InputBuiltin builtinId) st0
              let target = OutputTarget info layout []
              pure ([target], [varId], st1)
            Nothing -> do
              let loc = fromMaybe 0 entry.epReturnLocation
              (info, varId, st1) <- emitOutputVar StageFragment "frag_output" layout entry.epReturnAttrs (InputLocation loc) st0
              let target = OutputTarget info layout []
              pure ([target], [varId], st1)
    _ -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)

emitVertexOutput :: [(Text, StructDecl)] -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitVertexOutput structEnv entry retLayout st0 =
  case (retLayout, entry.epReturnType) of
    (Nothing, _) -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)
    (Just layout, Just ty) ->
      case ty of
        TyStructRef structName -> do
          when (isJust entry.epReturnBuiltin || isJust entry.epReturnLocation || not (null entry.epReturnAttrs)) $
            Left (CompileError "struct return values cannot use @location or @builtin on the function" Nothing Nothing)
          structDecl <- case lookup structName structEnv of
            Nothing -> Left (CompileError ("unknown struct: " <> textToString structName) Nothing Nothing)
            Just decl -> Right decl
          emitStructOutputs StageVertex structName layout structDecl st0
        _ -> do
          when (isJust entry.epReturnLocation) $
            Left (CompileError "vertex entry points do not support @location returns" Nothing Nothing)
          when (isNothing entry.epReturnBuiltin) $
            Left (CompileError "vertex entry point must return @builtin(position)" Nothing Nothing)
          case entry.epReturnBuiltin of
            Just "position" -> do
              when (TyVector 4 F32 /= ty) $
                Left (CompileError "@builtin(position) must be vec4<f32>" Nothing Nothing)
              (info, varId, st1) <- emitOutputVar StageVertex "position" layout entry.epReturnAttrs (InputBuiltin builtInPosition) st0
              let target = OutputTarget info layout []
              pure ([target], [varId], st1)
            _ -> Left (CompileError "vertex entry point must return @builtin(position)" Nothing Nothing)
    _ -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)

emitStructOutputs :: Stage -> Text -> TypeLayout -> StructDecl -> GenState -> Either CompileError ([OutputTarget], [Word32], GenState)
emitStructOutputs stage structName layout structDecl st0 =
  case layout of
    TLStruct _ fieldLayouts _ _ -> do
      let fields = structDecl.sdFields
      when (length fields /= length fieldLayouts) $
        Left (CompileError ("struct layout mismatch for " <> textToString structName) Nothing Nothing)
      when (stage == StageFragment) $
        validateBlendSrcStructRules fields
      let go _idx accTargets accIds usedLocs usedBuiltins st [] = Right (reverse accTargets, reverse accIds, st, usedLocs, usedBuiltins)
          go idx accTargets accIds usedLocs usedBuiltins st (field:rest) = do
            let fieldName = field.fdName
                attrs = field.fdAttrs
                fty = field.fdType
                layoutField = fieldLayouts !! idx
                fieldLayout = layoutField.flType
            when (containsResource fieldLayout) $
              Left (CompileError "resource types are not allowed as stage outputs" Nothing Nothing)
            when (containsAtomic fieldLayout) $
              Left (CompileError "atomic types are not allowed as stage outputs" Nothing Nothing)
            when (isJust (attrBuiltin attrs) && isJust (attrLocation attrs)) $
              Left (CompileError "struct fields cannot have both @location and @builtin" Nothing Nothing)
            case attrBuiltin attrs of
              Just builtin -> do
                when (builtin `elem` usedBuiltins) $
                  Left (CompileError ("duplicate @builtin(" <> textToString builtin <> ") on stage outputs") Nothing Nothing)
                expected <- case builtinOutputType stage builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage output") Nothing Nothing)
                  Just t -> Right t
                when (expected /= fty) $
                  Left (CompileError ("@builtin(" <> textToString builtin <> ") has wrong type") Nothing Nothing)
                builtinId <- case builtinOutputDecoration stage builtin of
                  Nothing -> Left (CompileError ("unsupported @builtin(" <> textToString builtin <> ") for stage output") Nothing Nothing)
                  Just bid -> Right bid
                (info, varId, st1) <- emitOutputVar stage (structName <> "_" <> fieldName) fieldLayout attrs (InputBuiltin builtinId) st
                let target = OutputTarget info fieldLayout [fromIntegral idx]
                go (idx + 1) (target:accTargets) (varId:accIds) usedLocs (builtin:usedBuiltins) st1 rest
              Nothing -> do
                loc <- case attrLocation attrs of
                  Nothing -> Left (CompileError "struct output fields must use @location or @builtin" Nothing Nothing)
                  Just n -> Right n
                when (stage == StageCompute) $
                  Left (CompileError "compute outputs cannot use @location" Nothing Nothing)
                blendSrc <-
                  if stage == StageFragment
                    then parseBlendSrcAttr attrs
                    else Right Nothing
                let blendIx = fromMaybe 0 blendSrc
                    locKey = (loc, blendIx)
                when (locKey `elem` usedLocs) $
                  Left (CompileError "duplicate @location/@blend_src on stage outputs" Nothing Nothing)
                (info, varId, st1) <- emitOutputVar stage (structName <> "_" <> fieldName) fieldLayout attrs (InputLocation loc) st
                let target = OutputTarget info fieldLayout [fromIntegral idx]
                go (idx + 1) (target:accTargets) (varId:accIds) (locKey:usedLocs) usedBuiltins st1 rest
      (targets, ids, st1, _usedLocs, _usedBuiltins) <- go 0 [] [] [] [] st0 fields
      case stage of
        StageVertex ->
          if any (\f -> attrBuiltin (f.fdAttrs) == Just "position") fields
            then pure ()
            else Left (CompileError "vertex output struct must include @builtin(position)" Nothing Nothing)
        _ -> pure ()
      pure (targets, ids, st1)
    _ -> Left (CompileError "expected struct return type" Nothing Nothing)
  where
    validateBlendSrcStructRules fields = do
      infos <- mapM fieldInfo fields
      let usesBlendSrc = any (isJust . third3) infos
      when usesBlendSrc $ do
        let allLocation0 = all (\(mLoc, _mBuiltin, _mBlend) -> mLoc == Just 0) infos
            noBuiltins = all (\(_mLoc, mBuiltin, _mBlend) -> isNothing mBuiltin) infos
            blendIdxs = [idx | (_mLoc, _mBuiltin, Just idx) <- infos]
            allHaveBlend = length blendIdxs == length infos
            exactlyTwo = length infos == 2
            bothIndices = Set.fromList blendIdxs == Set.fromList [0, 1]
        unless (exactlyTwo && noBuiltins && allHaveBlend && allLocation0 && bothIndices) $
          Left (CompileError "if @blend_src is used, fragment outputs must be exactly two @location(0) fields with @blend_src(0) and @blend_src(1)" Nothing Nothing)

    fieldInfo field = do
      let attrs = field.fdAttrs
      mBlend <- parseBlendSrcAttr attrs
      pure (attrLocation attrs, attrBuiltin attrs, mBlend)

    third3 (_a, _b, c) = c

emitOutputVar :: Stage -> Text -> TypeLayout -> [Attr] -> InputDecoration -> GenState -> Either CompileError (VarInfo, Word32, GenState)
emitOutputVar stage name layout attrs deco st0 = do
  validateUserStageIOType deco layout
  let (storageLayout, path) = sampleMaskLayout layout deco
  let (baseTy, st1) = emitTypeFromLayout st0 storageLayout
  let (ptrTy, st2) = emitPointerType st1 storageClassOutput baseTy
  let (varId, st3) = freshId st2
  let st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassOutput]) st3
  let st5a = case deco of
        InputBuiltin bid -> addDecoration (Instr opDecorate [varId, decorationBuiltIn, bid]) st4
        InputLocation loc -> addDecoration (Instr opDecorate [varId, decorationLocation, loc]) st4
  st5 <- applyIOAttrDecorations stage True layout attrs deco varId st5a
  let st6 = addName (Instr opName (varId : encodeString (textToString name))) st5
  let info = VarInfo layout varId storageClassOutput ReadWrite path
  pure (info, varId, st6)

sampleMaskLayout :: TypeLayout -> InputDecoration -> (TypeLayout, [Word32])
sampleMaskLayout layout deco =
  case deco of
    InputBuiltin bid
      | bid == builtInSampleMask
      , isSampleMaskScalar layout ->
          let (a, sz) = scalarLayout U32
              elemLayout = TLScalar U32 a sz
          in (TLArray (Just 1) sz elemLayout a sz, [0])
    _ -> (layout, [])
  where
    isSampleMaskScalar tl =
      case tl of
        TLScalar U32 _ _ -> True
        _ -> False

validateUserStageIOType :: InputDecoration -> TypeLayout -> Either CompileError ()
validateUserStageIOType deco layout =
  case deco of
    InputBuiltin _ -> Right ()
    InputLocation _ ->
      if isUserStageIOType layout
        then Right ()
        else Left (CompileError "stage @location inputs/outputs must be scalar or vector i32/u32/f16/f32" Nothing Nothing)

isUserStageIOType :: TypeLayout -> Bool
isUserStageIOType layout =
  case layout of
    TLScalar scalar _ _ -> isUserStageIOScalar scalar
    TLVector _ scalar _ _ -> isUserStageIOScalar scalar
    _ -> False

isUserStageIOScalar :: Scalar -> Bool
isUserStageIOScalar scalar =
  case scalar of
    I32 -> True
    U32 -> True
    F16 -> True
    F32 -> True
    _ -> False

isFloatScalarOrVector :: TypeLayout -> Bool
isFloatScalarOrVector layout =
  case layout of
    TLScalar F16 _ _ -> True
    TLScalar F32 _ _ -> True
    TLVector _ F16 _ _ -> True
    TLVector _ F32 _ _ -> True
    _ -> False

data InterpolateKind = InterpPerspective | InterpLinear | InterpFlat
  deriving (Eq, Show)

data InterpolateSampling = InterpCenter | InterpCentroid | InterpSample
  deriving (Eq, Show)

applyIOAttrDecorations :: Stage -> Bool -> TypeLayout -> [Attr] -> InputDecoration -> Word32 -> GenState -> Either CompileError GenState
applyIOAttrDecorations stage isOutput layout attrs deco varId st0 = do
  mInterpolate <- parseInterpolateAttr attrs
  hasInvariant <- parseInvariantAttr attrs
  mBlendSrc <- parseBlendSrcAttr attrs
  st1 <- applyInterpolate mInterpolate st0
  st2 <- applyBlendSrc mBlendSrc st1
  applyInvariant hasInvariant st2
  where
    isBuiltin =
      case deco of
        InputBuiltin _ -> True
        InputLocation _ -> False

    isInterpolationSite =
      (stage == StageVertex && isOutput)
        || (stage == StageFragment && not isOutput)

    applyInterpolate Nothing st = Right st
    applyInterpolate (Just (kind, sampling)) st = do
      when isBuiltin $
        Left (CompileError "@interpolate is not allowed on @builtin variables" Nothing Nothing)
      unless isInterpolationSite $
        Left (CompileError "@interpolate is only allowed on vertex outputs and fragment inputs" Nothing Nothing)
      when (kind /= InterpFlat && not (isFloatScalarOrVector layout)) $
        Left (CompileError "@interpolate(perspective|linear, ...): only floating-point scalars/vectors are allowed" Nothing Nothing)
      let st1 =
            case kind of
              InterpPerspective -> st
              InterpLinear -> addDecoration (Instr opDecorate [varId, decorationNoPerspective]) st
              InterpFlat -> addDecoration (Instr opDecorate [varId, decorationFlat]) st
      let st2 =
            case sampling of
              InterpCenter -> st1
              InterpCentroid -> addDecoration (Instr opDecorate [varId, decorationCentroid]) st1
              InterpSample -> addDecoration (Instr opDecorate [varId, decorationSample]) st1
      pure st2

    applyBlendSrc Nothing st = Right st
    applyBlendSrc (Just idx) st = do
      unless isOutput $
        Left (CompileError "@blend_src is only allowed on stage outputs" Nothing Nothing)
      when (stage /= StageFragment) $
        Left (CompileError "@blend_src is only allowed on fragment outputs" Nothing Nothing)
      unless (Set.member "dual_source_blending" st.gsEnabledFeatures) $
        Left (CompileError "@blend_src requires `enable dual_source_blending;`" Nothing Nothing)
      when isBuiltin $
        Left (CompileError "@blend_src is not allowed on @builtin variables" Nothing Nothing)
      case deco of
        InputLocation loc -> do
          when (loc /= 0) $
            Left (CompileError "@blend_src is only valid on @location(0)" Nothing Nothing)
          pure (addDecoration (Instr opDecorate [varId, decorationIndex, idx]) st)
        InputBuiltin _ ->
          Left (CompileError "@blend_src requires @location" Nothing Nothing)

    applyInvariant False st = Right st
    applyInvariant True st =
      case deco of
        InputBuiltin bid
          | stage == StageVertex
          , isOutput
          , bid == builtInPosition ->
              Right (addDecoration (Instr opDecorate [varId, decorationInvariant]) st)
          | stage == StageFragment
          , not isOutput
          , bid == builtInFragCoord ->
              Right (addDecoration (Instr opDecorate [varId, decorationInvariant]) st)
        _ ->
          Left (CompileError "@invariant is only allowed on @builtin(position) vertex outputs and fragment inputs" Nothing Nothing)

parseInterpolateAttr :: [Attr] -> Either CompileError (Maybe (InterpolateKind, InterpolateSampling))
parseInterpolateAttr attrs =
  case [args | Attr name args <- attrs, name == "interpolate"] of
    [] -> Right Nothing
    [args] -> Just <$> parseArgs args
    _ -> Left (CompileError "duplicate @interpolate attributes" Nothing Nothing)
  where
    parseArgs args =
      case args of
        [AttrIdent kindName] -> do
          kind <- parseKind kindName
          pure (kind, InterpCenter)
        [AttrIdent kindName, AttrIdent samplingName] -> do
          kind <- parseKind kindName
          sampling <- parseSampling samplingName
          when (kind == InterpFlat && sampling /= InterpCenter) $
            Left (CompileError "@interpolate(flat, ...): only center sampling is allowed" Nothing Nothing)
          pure (kind, sampling)
        _ ->
          Left (CompileError "@interpolate expects one or two identifier arguments" Nothing Nothing)

    parseKind name =
      case name of
        "perspective" -> Right InterpPerspective
        "linear" -> Right InterpLinear
        "flat" -> Right InterpFlat
        _ -> Left (CompileError ("unknown interpolation type: " <> textToString name) Nothing Nothing)

    parseSampling name =
      case name of
        "center" -> Right InterpCenter
        "centroid" -> Right InterpCentroid
        "sample" -> Right InterpSample
        _ -> Left (CompileError ("unknown interpolation sampling: " <> textToString name) Nothing Nothing)

parseInvariantAttr :: [Attr] -> Either CompileError Bool
parseInvariantAttr attrs =
  case [args | Attr name args <- attrs, name == "invariant"] of
    [] -> Right False
    [[]] -> Right True
    [_] -> Left (CompileError "@invariant does not accept arguments" Nothing Nothing)
    _ -> Left (CompileError "duplicate @invariant attributes" Nothing Nothing)

parseBlendSrcAttr :: [Attr] -> Either CompileError (Maybe Word32)
parseBlendSrcAttr attrs =
  case [args | Attr name args <- attrs, name == "blend_src"] of
    [] -> Right Nothing
    [[AttrInt n]] -> do
      when (n < 0 || n > 1) $
        Left (CompileError "@blend_src index must be 0 or 1" Nothing Nothing)
      pure (Just (fromIntegral n))
    [[_]] -> Left (CompileError "@blend_src expects an integer argument" Nothing Nothing)
    [_] -> Left (CompileError "@blend_src expects exactly one argument" Nothing Nothing)
    _ -> Left (CompileError "duplicate @blend_src attributes" Nothing Nothing)

emitMainFunction :: EntryPoint -> [(Text, VarInfo)] -> [EntryParamInit] -> [OutputTarget] -> GenState -> Either CompileError GenState
emitMainFunction entry env entryInits outTargets st0 = do
  let (voidTy, st1) = emitVoidType st0
  let (fnTy, st2) = emitFunctionType st1 voidTy []
  let (fnId, st3) = freshId st2
  let (labelId, st4) = freshId st3
  let st5 = addName (Instr opName (fnId : encodeString (textToString entry.epName))) st4
  let fs0 = FuncState [] [] env (mapFromAssocFirstWins env) [] Map.empty False [] []
  (st6, fs1) <- emitEntryParamInits entryInits st5 fs0
  (st7, fs2) <- emitStatements entry outTargets st6 fs1
  let tailInstrs =
        if fs2.fsTerminated
          then [Instr opFunctionEnd []]
          else [Instr opReturn [], Instr opFunctionEnd []]
  let funcInstrs = finalizeFunctionInstrs
        [ Instr opFunction [voidTy, fnId, functionControlNone, fnTy]
        , Instr opLabel [labelId]
        ]
        fs2
        tailInstrs
  let st8 = addFunctions funcInstrs st7
  let st9 = st8 { gsEntryPoint = Just fnId }
  pure st9

emitEntryParamInits :: [EntryParamInit] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitEntryParamInits inits st fs = foldM emitOne (st, fs) inits
  where
    emitOne (st', fs') initParam = do
      (st1, fs1, fieldVals) <- emitEntryFieldValues st' fs' (initParam.epiFields)
      let (tyId, st2) = emitTypeFromLayout st1 (initParam.epiLayout)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) fieldVals)) fs1
      let (ptrTy, st4) = emitPointerType st3 storageClassFunction tyId
      let (varId, st5) = freshId st4
      let fs3 = addFuncLocal (Instr opVariable [ptrTy, varId, storageClassFunction]) fs2
      let fs4 = addFuncInstr (Instr opStore [varId, resId]) fs3
      let info = VarInfo (initParam.epiLayout) varId storageClassFunction ReadOnly []
      let fs5 = fs4
            { fsVars = (initParam.epiName, info) : fs4.fsVars
            , fsVarsByName = Map.insert initParam.epiName info fs4.fsVarsByName
            }
      pure (st5, fs5)

emitEntryFieldValues :: GenState -> FuncState -> [EntryFieldInit] -> Either CompileError (GenState, FuncState, [Value])
emitEntryFieldValues st fs fields = go st fs fields []
  where
    go st' fs' [] acc = Right (st', fs', reverse acc)
    go st' fs' (EntryFieldInit _ info:rest) acc = do
      (st1, fs1, val) <- emitLoadVar st' fs' info
      go st1 fs1 rest (val:acc)

emitLoadVar :: GenState -> FuncState -> VarInfo -> Either CompileError (GenState, FuncState, Value)
emitLoadVar = emitLoadFromPtr

registerFunctions :: StructLayoutCache -> [FunctionDecl] -> GenState -> Either CompileError GenState
registerFunctions layoutCache decls st0 = foldM registerOne st0 decls
  where
    registerOne st decl =
      do
        paramLayouts <- mapM (resolveTypeLayoutWithCache layoutCache) (map (.paramType) decl.fnParams)
        mapM_ (ensureNoResources "function parameter") paramLayouts
        retLayout <- case decl.fnReturnType of
          Nothing -> Right Nothing
          Just ty -> do
            layout <- resolveTypeLayoutWithCache layoutCache ty
            ensureNoResources "function return type" layout
            Right (Just layout)
        let existing = Map.findWithDefault [] decl.fnName st.gsFunctionsByName
        when (any (\fi -> fi.fiParams == paramLayouts) existing) $
          Left (CompileError ("duplicate function overload: " <> textToString decl.fnName) Nothing Nothing)
        let (retTyId, st1) = case retLayout of
              Nothing -> emitVoidType st
              Just layout -> emitTypeFromLayout st layout
        let (st2, paramTypeIds) =
              mapAccumL (\acc layout -> let (tid, acc') = emitTypeFromLayout acc layout in (acc', tid)) st1 paramLayouts
        let (fnTypeId, st3) = emitFunctionType st2 retTyId paramTypeIds
        let (fnId, st4) = freshId st3
        let info = FunctionInfo decl.fnName paramLayouts retLayout fnId fnTypeId
        let st5 = st4
              { gsFunctionTable = info : st4.gsFunctionTable
              , gsFunctionsByName = Map.insertWith (++) decl.fnName [info] st4.gsFunctionsByName
              }
        pure st5

emitFunctionBodies :: StructLayoutCache -> [FunctionDecl] -> GenState -> Either CompileError GenState
emitFunctionBodies layoutCache decls st0 = foldM emitOne st0 decls
  where
    emitOne st decl = do
      paramLayouts <- mapM (resolveTypeLayoutWithCache layoutCache) (map (.paramType) decl.fnParams)
      case findFunctionInfo decl.fnName paramLayouts st of
        Nothing -> Left (CompileError ("missing function info for " <> textToString decl.fnName) Nothing Nothing)
        Just info -> emitFunctionBody info decl st

findFunctionInfo :: Text -> [TypeLayout] -> GenState -> Maybe FunctionInfo
findFunctionInfo name paramLayouts st =
  case filter (\fi -> fi.fiParams == paramLayouts) (Map.findWithDefault [] name st.gsFunctionsByName) of
    (x:_) -> Just x
    [] -> Nothing

emitFunctionBody :: FunctionInfo -> FunctionDecl -> GenState -> Either CompileError GenState
emitFunctionBody info decl st0 = do
  let (retTyId, st1) = case info.fiReturn of
        Nothing -> emitVoidType st0
        Just layout -> emitTypeFromLayout st0 layout
  let (fnLabel, st2) = freshId st1
  let st3 = addName (Instr opName (info.fiId : encodeString (textToString decl.fnName))) st2
  let (paramInstrs, paramLocals, paramStores, env, st4) = emitFunctionParams decl.fnParams info.fiParams st3
  let envWithGlobals = st4.gsGlobalVars <> env
  let fs0 = FuncState (reverse paramLocals) (reverse paramStores) envWithGlobals (mapFromAssocFirstWins envWithGlobals) [] Map.empty False [] []
  (st5, fs1) <- emitStmtListFn info.fiReturn st4 fs0 decl.fnBody
  fs2 <- finalizeFunctionReturn info.fiReturn fs1
  let funcInstrs = finalizeFunctionInstrs
        ([Instr opFunction [retTyId, info.fiId, functionControlNone, info.fiTypeId]] <> paramInstrs <> [Instr opLabel [fnLabel]])
        fs2
        [Instr opFunctionEnd []]
  let st6 = addFunctions funcInstrs st5
  pure st6

emitFunctionParams :: [Param] -> [TypeLayout] -> GenState -> ([Instr], [Instr], [Instr], [(Text, VarInfo)], GenState)
emitFunctionParams params layouts st0 =
  let go st accInstrs accLocals accStores accEnv [] [] = (reverse accInstrs, reverse accLocals, reverse accStores, reverse accEnv, st)
      go st accInstrs accLocals accStores accEnv (p:ps) (l:ls) =
        let (paramTyId, st1) = emitTypeFromLayout st l
            (paramId, st2) = freshId st1
            paramInstr = Instr opFunctionParameter [paramTyId, paramId]
            (ptrTy, st3) = emitPointerType st2 storageClassFunction paramTyId
            (varId, st4) = freshId st3
            localInstr = Instr opVariable [ptrTy, varId, storageClassFunction]
            storeInstr = Instr opStore [varId, paramId]
            info = VarInfo l varId storageClassFunction ReadOnly []
        in go st4 (paramInstr:accInstrs) (localInstr:accLocals) (storeInstr:accStores) ((p.paramName, info):accEnv) ps ls
      go st accInstrs accLocals accStores accEnv _ _ = (reverse accInstrs, reverse accLocals, reverse accStores, reverse accEnv, st)
  in go st0 [] [] [] [] params layouts

emitStmtListFn :: Maybe TypeLayout -> GenState -> FuncState -> [Stmt] -> Either CompileError (GenState, FuncState)
emitStmtListFn retLayout = go
  where
    go st' fs' [] = Right (st', fs')
    go st' fs' (s:ss) = do
      (st1, fs1) <- emitStmtFn retLayout st' fs' s
      go st1 fs1 ss

emitStmtFn :: Maybe TypeLayout -> GenState -> FuncState -> Stmt -> Either CompileError (GenState, FuncState)
emitStmtFn retLayout st fs stmt
  | fs.fsTerminated = Right (st, fs)
  | otherwise =
      case stmt of
        SLet _ name mType expr -> emitLet name mType expr st fs
        SVar _ name mType mExpr -> emitVar name mType mExpr st fs
        SAssign _ lv expr -> emitAssignStmt lv expr st fs
        SAssignOp _ lv op expr -> emitAssignOpStmt lv op expr st fs
        SInc _ lv -> emitIncDecStmt OpAdd lv st fs
        SDec _ lv -> emitIncDecStmt OpSub lv st fs
        SExpr _ expr -> emitExprStmt st fs expr
        SIf _ cond thenBody elseBody ->
          emitIfFn retLayout cond thenBody elseBody st fs
        SWhile _ cond body ->
          emitWhileFn retLayout cond body st fs
        SLoop _ body continuing ->
          emitLoopFn retLayout body continuing st fs
        SFor _ initStmt condExpr contStmt body ->
          emitForFn retLayout initStmt condExpr contStmt body st fs
        SSwitch _ expr cases defBody ->
          emitSwitchFn retLayout expr cases defBody st fs
        SBreak _ ->
          emitBreak st fs
        SBreakIf pos cond ->
          emitIfFn retLayout cond [SBreak pos] Nothing st fs
        SContinue _ ->
          emitContinue st fs
        SDiscard _ ->
          if st.gsEntryStage == StageFragment
            then
              let fs1 = addTerminator (Instr opKill []) fs
              in Right (st, fs1)
            else Left (CompileError "discard is only allowed in fragment entry points" Nothing Nothing)
        SFallthrough _ ->
          Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
        SReturn _ mexpr ->
          case retLayout of
            Nothing ->
              case mexpr of
                Nothing -> Right (st, terminateWithReturn fs)
                Just _ -> Left (CompileError "void function cannot return a value" Nothing Nothing)
            Just layout -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "non-void function must return a value" Nothing Nothing)
                Just e -> Right e
              (st1, fs1, val) <- emitExpr st fs expr
              (st2, fs2, val') <- coerceValueToLayout layout val st1 fs1
              let fs3 = addFuncInstr (Instr opReturnValue [val'.valId]) fs2
              Right (st2, fs3 { fsTerminated = True })

finalizeFunctionReturn :: Maybe TypeLayout -> FuncState -> Either CompileError FuncState
finalizeFunctionReturn retLayout fs =
  if fs.fsTerminated
    then Right fs
    else case retLayout of
      Nothing -> Right (terminateWithReturn fs)
      Just _ -> Left (CompileError "non-void function must return a value" Nothing Nothing)

emitIfFn :: Maybe TypeLayout -> Expr -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitIfFn retLayout cond thenBody elseBody st fs = do
  (st1, fs1, condVal) <- emitExpr st fs cond
  ensureBoolScalar (condVal.valType)
  let (thenLabel, st2) = freshId st1
  let (elseLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3
  let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
  let fs3 = addTerminator (Instr opBranchConditional [condVal.valId, thenLabel, elseLabel]) fs2

  let fsThen0 = addLabel thenLabel fs3
  (st5, fsThen1) <- emitStmtListFn retLayout st4 fsThen0 thenBody
  let thenTerm = fsThen1.fsTerminated
  let fsThen2 = if thenTerm then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

  let fsElse0 = addLabel elseLabel fsThen2
  (st6, fsElse1) <- case elseBody of
    Nothing -> Right (st5, fsElse0)
    Just body -> emitStmtListFn retLayout st5 fsElse0 body
  let elseTerm = fsElse1.fsTerminated
  let fsElse2 = if elseTerm then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

  let fsMerge = addLabel mergeLabel fsElse2
  let fsMerge1 =
        if thenTerm && elseTerm
          then addTerminator (Instr opUnreachable []) fsMerge
          else fsMerge
  Right (st6, fsMerge1)

emitWhileFn :: Maybe TypeLayout -> Expr -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitWhileFn retLayout cond body st fs = do
  let loopStack = fs.fsLoopStack
  let breakStack = fs.fsBreakStack
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  (st5, fsHeader1, condVal) <- emitExpr st4 fsHeader0 cond
  ensureBoolScalar (condVal.valType)
  let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
  let fsHeader3 = addTerminator (Instr opBranchConditional [condVal.valId, bodyLabel, mergeLabel]) fsHeader2

  let fsBody0 = addLabel bodyLabel fsHeader3
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsBody2) <- emitStmtListFn retLayout st5 fsBody1 body
  let fsBody3 = if fsBody2.fsTerminated then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = addTerminator (Instr opBranch [headerLabel]) (fsContinue0 { fsBreakStack = mergeLabel : breakStack })

  let fsMerge = addLabel mergeLabel fsContinue1
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitLoopFn :: Maybe TypeLayout -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLoopFn retLayout body continuing st fs = do
  let loopStack = fs.fsLoopStack
  let breakStack = fs.fsBreakStack
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  let fsHeader1 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader0
  let fsHeader2 = addTerminator (Instr opBranch [bodyLabel]) fsHeader1

  let fsBody0 = addLabel bodyLabel fsHeader2
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st5, fsBody2) <- emitStmtListFn retLayout st4 fsBody1 body
  let fsBody3 = if fsBody2.fsTerminated then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsContinue2) <- case continuing of
    Nothing -> Right (st5, fsContinue1)
    Just contBody -> emitStmtListFn retLayout st5 fsContinue1 contBody
  let fsContinue3 =
        if fsContinue2.fsTerminated
          then fsContinue2
          else addTerminator (Instr opBranch [headerLabel]) fsContinue2

  let fsMerge = addLabel mergeLabel fsContinue3
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitForFn :: Maybe TypeLayout -> Maybe Stmt -> Maybe Expr -> Maybe Stmt -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitForFn retLayout initStmt condExpr contStmt body st fs = do
  (st1, fs1) <- case initStmt of
    Nothing -> Right (st, fs)
    Just s -> emitStmtFn retLayout st fs s
  if fs1.fsTerminated
    then Right (st1, fs1)
    else do
      let loopStack = fs1.fsLoopStack
      let breakStack = fs1.fsBreakStack
      let (headerLabel, st2) = freshId st1
      let (bodyLabel, st3) = freshId st2
      let (continueLabel, st4) = freshId st3
      let (mergeLabel, st5) = freshId st4

      let fs2 = addTerminator (Instr opBranch [headerLabel]) fs1
      let fsHeader0 = addLabel headerLabel fs2
      (st6, fsHeader1, condVal) <- case condExpr of
        Nothing -> do
          let (cid, st') = emitConstBool st5 True
          let (a, sz) = scalarLayout Bool
          let layout = TLScalar Bool a sz
          Right (st', fsHeader0, Value layout cid)
        Just expr -> emitExpr st5 fsHeader0 expr
      ensureBoolScalar (condVal.valType)
      let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
      let fsHeader3 = addTerminator (Instr opBranchConditional [condVal.valId, bodyLabel, mergeLabel]) fsHeader2

      let fsBody0 = addLabel bodyLabel fsHeader3
      let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st7, fsBody2) <- emitStmtListFn retLayout st6 fsBody1 body
      let fsBody3 = if fsBody2.fsTerminated then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

      let fsContinue0 = addLabel continueLabel fsBody3
      let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st8, fsContinue2) <- case contStmt of
        Nothing -> Right (st7, fsContinue1)
        Just s -> emitStmtFn retLayout st7 fsContinue1 s
      let fsContinue3 =
            if fsContinue2.fsTerminated
              then fsContinue2
              else addTerminator (Instr opBranch [headerLabel]) fsContinue2

      let fsMerge = addLabel mergeLabel fsContinue3
      let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
      Right (st8, fsMerge1)

emitSwitchFn :: Maybe TypeLayout -> Expr -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitchFn retLayout expr cases defBody st fs = do
  (st1, fs1, selVal) <- emitExpr st fs expr
  ensureSwitchType (selVal.valType)
  cases' <- expandSwitchCases cases defBody
  let breakStack = fs1.fsBreakStack
  let (mergeLabel, st2) = freshId st1
  let fs2 = fs1 { fsBreakStack = mergeLabel : breakStack }
  (st3, fs3) <- emitSwitchChainFn retLayout selVal cases' defBody st2 fs2
  let fs4 = if fs3.fsTerminated then fs3 else addTerminator (Instr opBranch [mergeLabel]) fs3
  let fs5 = addLabel mergeLabel fs4
  let fs6 = fs5 { fsBreakStack = breakStack }
  Right (st3, fs6)

emitSwitchChainFn :: Maybe TypeLayout -> Value -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitchChainFn retLayout selVal cases defBody st fs =
  case cases of
    [] ->
      case defBody of
        Nothing -> Right (st, fs)
        Just body -> emitStmtListFn retLayout st fs body
    (SwitchCase selectors body : rest) -> do
      (st1, fs1, condVal) <- emitSwitchCond selVal selectors st fs
      ensureBoolScalar (condVal.valType)
      let (thenLabel, st2) = freshId st1
      let (elseLabel, st3) = freshId st2
      let (mergeLabel, st4) = freshId st3
      let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
      let fs3 = addTerminator (Instr opBranchConditional [condVal.valId, thenLabel, elseLabel]) fs2

      let fsThen0 = addLabel thenLabel fs3
      (st5, fsThen1) <- emitStmtListFn retLayout st4 fsThen0 body
      let fsThen2 = if fsThen1.fsTerminated then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

      let fsElse0 = addLabel elseLabel fsThen2
      (st6, fsElse1) <- emitSwitchChainFn retLayout selVal rest defBody st5 fsElse0
      let fsElse2 = if fsElse1.fsTerminated then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

      let fsMerge = addLabel mergeLabel fsElse2
      let fsMerge1 =
            if fsThen1.fsTerminated && fsElse1.fsTerminated
              then addTerminator (Instr opUnreachable []) fsMerge
              else fsMerge
      Right (st6, fsMerge1)

emitSwitchCond :: Value -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSwitchCond selVal selectors st fs =
  case selectors of
    [] -> Left (CompileError "switch case must have at least one selector" Nothing Nothing)
    firstSel : rest -> do
      (st1, fs1, firstCond) <- emitSelector selVal firstSel st fs
      foldM combine (st1, fs1, firstCond) rest
  where
    emitSelector base expr st' fs' = do
      (st1, sel) <- emitConstExpr st' expr
      (st2, sel') <- coerceConstValueToLayout (base.valType) sel st1
      emitBinary OpEq (base.valType) (base.valId) (sel'.valId) st2 fs'
    combine (st1, fs1, acc) expr = do
      (st2, fs2, nextCond) <- emitSelector selVal expr st1 fs1
      emitBinary OpOr (acc.valType) (acc.valId) (nextCond.valId) st2 fs2

ensureNoResources :: String -> TypeLayout -> Either CompileError ()
ensureNoResources label layout =
  if containsResource layout || containsAtomic layout
    then Left (CompileError (label <> " cannot contain resource or atomic types") Nothing Nothing)
    else Right ()

addFunctions :: [Instr] -> GenState -> GenState
addFunctions instrs st = st { gsFunctions = reverse instrs <> st.gsFunctions }

getExtInstSet :: GenState -> String -> (Word32, GenState)
getExtInstSet st name =
  case lookup name (st.gsExtInstIds) of
    Just sid -> (sid, st)
    Nothing ->
      let (sid, st1) = freshId st
          instr = Instr opExtInstImport (sid : encodeString name)
          st2 = st1
            { gsExtInstIds = (name, sid) : st1.gsExtInstIds
            , gsExtInstImports = instr : st1.gsExtInstImports
            }
      in (sid, st2)

addFuncInstr :: Instr -> FuncState -> FuncState
addFuncInstr instr fs = fs { fsInstrs = instr : fs.fsInstrs }

addFuncLocal :: Instr -> FuncState -> FuncState
addFuncLocal instr fs = fs { fsLocals = instr : fs.fsLocals }

addTerminator :: Instr -> FuncState -> FuncState
addTerminator instr fs = fs { fsInstrs = instr : fs.fsInstrs, fsTerminated = True }

addLabel :: Word32 -> FuncState -> FuncState
addLabel lbl fs = fs { fsInstrs = Instr opLabel [lbl] : fs.fsInstrs, fsTerminated = False }

terminateWithReturn :: FuncState -> FuncState
terminateWithReturn fs = fs { fsInstrs = Instr opReturn [] : fs.fsInstrs, fsTerminated = True }

finalizeFunctionInstrs :: [Instr] -> FuncState -> [Instr] -> [Instr]
finalizeFunctionInstrs prefix fs suffix =
  prefix <> reverse fs.fsLocals <> reverse fs.fsInstrs <> suffix

emitStatements :: EntryPoint -> [OutputTarget] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitStatements entry outTargets st fs = do
  (st1, fs1) <- emitStmtList entry outTargets st fs (entry.epBody)
  case entry.epStage of
    StageFragment ->
      if fs1.fsTerminated
        then Right (st1, fs1)
        else Left (CompileError "fragment entry point must return a value" Nothing Nothing)
    StageVertex ->
      if fs1.fsTerminated
        then Right (st1, fs1)
        else Left (CompileError "vertex entry point must return a value" Nothing Nothing)
    StageCompute -> Right (st1, fs1)

storeReturnValue :: [OutputTarget] -> TypeLayout -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
storeReturnValue targets valLayout valueId st fs = do
  when (null targets) $
    Left (CompileError "missing output targets for return value" Nothing Nothing)
  case targets of
    [OutputTarget out layout []] -> do
      ensureTypeMatch layout valLayout
      emitStoreToVar st fs out valueId
    _ -> foldM storeOne (st, fs) targets
  where
    storeOne (st', fs') target = do
      let outInfo = target.otVar
      let outLayout = target.otLayout
      case target.otPath of
        [] -> do
          ensureTypeMatch outLayout valLayout
          emitStoreToVar st' fs' outInfo valueId
        path -> do
          let (tyId, st1) = emitTypeFromLayout st' outLayout
          let (resId, st2) = freshId st1
          let fs1 = addFuncInstr (Instr opCompositeExtract (tyId : resId : valueId : path)) fs'
          emitStoreToVar st2 fs1 outInfo resId

emitStmtList :: EntryPoint -> [OutputTarget] -> GenState -> FuncState -> [Stmt] -> Either CompileError (GenState, FuncState)
emitStmtList entry outTargets = go
  where
    go st' fs' [] = Right (st', fs')
    go st' fs' (s:ss) = do
      (st1, fs1) <- emitStmt entry outTargets st' fs' s
      go st1 fs1 ss

emitStmt :: EntryPoint -> [OutputTarget] -> GenState -> FuncState -> Stmt -> Either CompileError (GenState, FuncState)
emitStmt entry outTargets st fs stmt
  | fs.fsTerminated = Right (st, fs)
  | otherwise =
      case stmt of
        SLet _ name mType expr -> emitLet name mType expr st fs
        SVar _ name mType mExpr -> emitVar name mType mExpr st fs
        SAssign _ lv expr -> emitAssignStmt lv expr st fs
        SAssignOp _ lv op expr -> emitAssignOpStmt lv op expr st fs
        SInc _ lv -> emitIncDecStmt OpAdd lv st fs
        SDec _ lv -> emitIncDecStmt OpSub lv st fs
        SExpr _ expr -> emitExprStmt st fs expr
        SIf _ cond thenBody elseBody ->
          emitIf entry outTargets cond thenBody elseBody st fs
        SWhile _ cond body ->
          emitWhile entry outTargets cond body st fs
        SLoop _ body continuing ->
          emitLoop entry outTargets body continuing st fs
        SFor _ initStmt condExpr contStmt body ->
          emitFor entry outTargets initStmt condExpr contStmt body st fs
        SSwitch _ expr cases defBody ->
          emitSwitch entry outTargets expr cases defBody st fs
        SBreak _ ->
          emitBreak st fs
        SBreakIf pos cond ->
          emitIf entry outTargets cond [SBreak pos] Nothing st fs
        SContinue _ ->
          emitContinue st fs
        SDiscard _ ->
          case entry.epStage of
            StageFragment ->
              let fs1 = addTerminator (Instr opKill []) fs
              in Right (st, fs1)
            _ -> Left (CompileError "discard is only allowed in fragment entry points" Nothing Nothing)
        SFallthrough _ ->
          Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
        SReturn _ mexpr ->
          case entry.epStage of
            StageCompute ->
              case mexpr of
                Nothing ->
                  let fs1 = addTerminator (Instr opReturn []) fs
                  in Right (st, fs1)
                Just _ -> Left (CompileError "compute entry points cannot return a value" Nothing Nothing)
            StageFragment -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "fragment entry points must return a value" Nothing Nothing)
                Just e -> Right e
              (st1, fs1, val) <- emitExpr st fs expr
              (st2, fs2) <- storeReturnValue outTargets (val.valType) (val.valId) st1 fs1
              let fs3 = addTerminator (Instr opReturn []) fs2
              Right (st2, fs3)
            StageVertex -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "vertex entry points must return a value" Nothing Nothing)
                Just e -> Right e
              (st1, fs1, val) <- emitExpr st fs expr
              (st2, fs2) <- storeReturnValue outTargets (val.valType) (val.valId) st1 fs1
              let fs3 = addTerminator (Instr opReturn []) fs2
              Right (st2, fs3)

withNonAtomicPtr :: GenState -> FuncState -> LValue -> (GenState -> FuncState -> VarInfo -> Either CompileError (GenState, FuncState)) -> Either CompileError (GenState, FuncState)
withNonAtomicPtr st fs lv k = do
  (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
  case ptrInfo.viType of
    TLAtomic _ -> Left (CompileError "use atomicStore for atomic values" Nothing Nothing)
    _ -> k st1 fs1 ptrInfo
{-# INLINE withNonAtomicPtr #-}

emitAssignStmt :: LValue -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitAssignStmt lv expr st fs =
  withNonAtomicPtr st fs lv $ \st1 fs1 ptrInfo -> do
    (st2, fs2, val) <- emitExpr st1 fs1 expr
    (st3, fs3, val') <- coerceValueToLayout (ptrInfo.viType) val st2 fs2
    ensureWritable ptrInfo
    emitStoreToVar st3 fs3 ptrInfo val'.valId
{-# INLINE emitAssignStmt #-}

emitAssignOpStmt :: LValue -> BinOp -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitAssignOpStmt lv op expr st fs =
  withNonAtomicPtr st fs lv $ \st1 fs1 ptrInfo -> do
    ensureWritable ptrInfo
    (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
    (st3, fs3, rhsVal) <- emitExpr st2 fs2 expr
    (st4, fs4, rhsVal') <- coerceValueToLayout (ptrInfo.viType) rhsVal st3 fs3
    (st5, fs5, resVal) <- emitBinary op (ptrInfo.viType) (lhsVal.valId) (rhsVal'.valId) st4 fs4
    emitStoreToVar st5 fs5 ptrInfo resVal.valId
{-# INLINE emitAssignOpStmt #-}

emitIncDecStmt :: BinOp -> LValue -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitIncDecStmt op lv st fs =
  withNonAtomicPtr st fs lv $ \st1 fs1 ptrInfo -> do
    ensureWritable ptrInfo
    (st2, fs2, lhsVal) <- emitLoadFromPtr st1 fs1 ptrInfo
    (oneId, st3) <- emitConstOne (ptrInfo.viType) st2
    (st4, fs3, resVal) <- emitBinary op (ptrInfo.viType) (lhsVal.valId) oneId st3 fs2
    emitStoreToVar st4 fs3 ptrInfo resVal.valId
{-# INLINE emitIncDecStmt #-}

emitExprStmt :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState)
emitExprStmt st fs expr =
  case expr of
    ECall _ name args ->
      case name of
        "textureStore" -> emitTextureStore args st fs
        "atomicStore" -> emitAtomicStore args st fs
        "workgroupBarrier" -> emitBarrierBuiltin "workgroupBarrier" memorySemanticsWorkgroupMemory args st fs
        "storageBarrier" -> emitBarrierBuiltin "storageBarrier" memorySemanticsUniformMemory args st fs
        "textureBarrier" -> emitBarrierBuiltin "textureBarrier" memorySemanticsImageMemory args st fs
        _ ->
          if Map.member name st.gsFunctionsByName
            then do
              (st1, fs1, _) <- emitFunctionCallByName name args st fs
              Right (st1, fs1)
            else do
              (st1, fs1, _) <- emitExpr st fs expr
              Right (st1, fs1)
    _ -> do
      (st1, fs1, _) <- emitExpr st fs expr
      Right (st1, fs1)

emitBarrierBuiltin :: String -> Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitBarrierBuiltin name semMask args st fs =
  case args of
    [] -> do
      let stageOk =
            case name of
              "workgroupBarrier" -> st.gsEntryStage == StageCompute
              "storageBarrier" -> st.gsEntryStage == StageCompute
              "textureBarrier" -> st.gsEntryStage == StageCompute
              _ -> False
      unless stageOk $
        Left (CompileError (name <> " is not available in this shader stage") Nothing Nothing)
      let (execScopeId, st1) = emitConstU32 st memoryScopeWorkgroup
      let (memScopeId, st2) = emitConstU32 st1 memoryScopeWorkgroup
      let (semanticsId, st3) = emitConstU32 st2 (memorySemanticsAcquireRelease .|. semMask)
      let fs1 = addFuncInstr (Instr opControlBarrier [execScopeId, memScopeId, semanticsId]) fs
      Right (st3, fs1)
    _ -> Left (CompileError (name <> " expects no arguments") Nothing Nothing)

emitLoadFromPtr :: GenState -> FuncState -> VarInfo -> Either CompileError (GenState, FuncState, Value)
emitLoadFromPtr st fs info = do
  (st1, fs1, ptrId) <- resolveVarPtr st fs info
  let (tyId, st2) = emitTypeFromLayout st1 (info.viType)
  let (resId, st3) = freshId st2
  let fs2 = addFuncInstr (Instr opLoad [tyId, resId, ptrId]) fs1
  Right (st3, fs2, Value (info.viType) resId)
{-# INLINE emitLoadFromPtr #-}

resolveVarPtr :: GenState -> FuncState -> VarInfo -> Either CompileError (GenState, FuncState, Word32)
resolveVarPtr st fs info =
  case info.viPath of
    [] -> Right (st, fs, info.viPtrId)
    path -> do
      let addIx (acc, ids) ix =
            let (cid, acc') = emitConstU32 acc ix
            in (acc', ids <> [cid])
      let (st1, indexIds) = foldl' addIx (st, []) path
      let (elemTy, st2) = emitTypeFromLayout st1 info.viType
      let (ptrTy, st3) = emitPointerType st2 info.viStorage elemTy
      let (ptrId, st4) = freshId st3
      let fs1 = addFuncInstr (Instr opAccessChain (ptrTy : ptrId : info.viPtrId : indexIds)) fs
      Right (st4, fs1, ptrId)
{-# INLINE resolveVarPtr #-}

emitStoreToVar :: GenState -> FuncState -> VarInfo -> Word32 -> Either CompileError (GenState, FuncState)
emitStoreToVar st fs info valueId = do
  (st1, fs1, ptrId) <- resolveVarPtr st fs info
  let fs2 = addFuncInstr (Instr opStore [ptrId, valueId]) fs1
  Right (st1, fs2)
{-# INLINE emitStoreToVar #-}

emitConstOne :: TypeLayout -> GenState -> Either CompileError (Word32, GenState)
emitConstOne layout st =
  case layout of
    TLScalar U32 _ _ -> Right (emitConstU32 st 1)
    TLScalar I32 _ _ -> Right (emitConstI32 st 1)
    TLScalar F32 _ _ -> Right (emitConstF32 st 1.0)
    TLScalar F16 _ _ -> Right (emitConstF16 st 1.0)
    _ -> Left (CompileError "increment/decrement requires an i32, u32, f16, or f32 scalar" Nothing Nothing)

resolveTypeLayoutInState :: GenState -> Type -> Either CompileError TypeLayout
resolveTypeLayoutInState st ty =
  let cache = Map.fromList st.gsStructLayouts
  in resolveTypeLayoutWithCache cache ty

emitZeroValue :: TypeLayout -> GenState -> Either CompileError (GenState, Value)
emitZeroValue layout st =
  case layout of
    TLScalar I32 _ _ ->
      let (cid, st1) = emitConstI32 st 0
      in Right (st1, Value layout cid)
    TLScalar U32 _ _ ->
      let (cid, st1) = emitConstU32 st 0
      in Right (st1, Value layout cid)
    TLScalar F32 _ _ ->
      let (cid, st1) = emitConstF32 st 0.0
      in Right (st1, Value layout cid)
    TLScalar F16 _ _ ->
      let (cid, st1) = emitConstF16 st 0.0
      in Right (st1, Value layout cid)
    TLScalar Bool _ _ ->
      let (cid, st1) = emitConstBool st False
      in Right (st1, Value layout cid)
    TLVector n scalar _ _ -> do
      let (a, sz) = scalarLayout scalar
      let scalarLayout' = TLScalar scalar a sz
      (st1, comps) <- buildComponents scalarLayout' n st
      let (cid, st2) = emitConstComposite layout (map (.valId) comps) st1
      Right (st2, Value layout cid)
    TLMatrix cols rows scalar _ _ _ -> do
      let (va, vsz) = vectorLayout scalar rows
      let vecLayout = TLVector rows scalar va vsz
      (st1, col) <- emitZeroValue vecLayout st
      let (cid, st2) = emitConstComposite layout (replicate cols col.valId) st1
      Right (st2, Value layout cid)
    TLArray (Just n) _ elemLayout _ _ -> do
      (st1, elems) <- buildArrayElems elemLayout n st
      let (cid, st2) = emitConstComposite layout (map (.valId) elems) st1
      Right (st2, Value layout cid)
    TLArray Nothing _ _ _ _ ->
      Left (CompileError "runtime arrays cannot be default-initialized" Nothing Nothing)
    TLStruct _ fields _ _ -> do
      (st1, elems) <- buildFieldElems fields st
      let (cid, st2) = emitConstComposite layout (map (.valId) elems) st1
      Right (st2, Value layout cid)
    TLAtomic _ -> Left (CompileError "atomic types cannot be default-initialized" Nothing Nothing)
    TLPointer {} -> Left (CompileError "pointer types cannot be default-initialized" Nothing Nothing)
    TLSampler -> Left (CompileError "sampler types cannot be default-initialized" Nothing Nothing)
    TLSamplerComparison -> Left (CompileError "sampler types cannot be default-initialized" Nothing Nothing)
    TLTexture1D _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTexture1DArray _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTexture2D _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTexture2DArray _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTexture3D _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureCube _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureCubeArray _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureMultisampled2D _ -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureDepth2D -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureDepth2DArray -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureDepthCube -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureDepthCubeArray -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLTextureDepthMultisampled2D -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLStorageTexture1D {} -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLStorageTexture2D {} -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLStorageTexture2DArray {} -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
    TLStorageTexture3D {} -> Left (CompileError "texture types cannot be default-initialized" Nothing Nothing)
  where
    buildComponents scalarLayout' count st' = go count st' []
      where
        go 0 st'' acc = Right (st'', reverse acc)
        go k st'' acc = do
          (st1, val) <- emitZeroValue scalarLayout' st''
          go (k - 1) st1 (val : acc)

    buildArrayElems elemLayout count st' = go count st' []
      where
        go 0 st'' acc = Right (st'', reverse acc)
        go k st'' acc = do
          (st1, val) <- emitZeroValue elemLayout st''
          go (k - 1) st1 (val : acc)

    buildFieldElems fields st' = go fields st' []
      where
        go [] st'' acc = Right (st'', reverse acc)
        go (f:fs') st'' acc = do
          (st1, val) <- emitZeroValue f.flType st''
          go fs' st1 (val : acc)

emitLet :: Text -> Maybe Type -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLet name mType expr st fs = do
  (st1, fs1, val0) <- emitExpr st fs expr
  (st2, fs2, val) <- case mType of
    Nothing -> Right (st1, fs1, val0)
    Just ty -> do
      layout <- resolveTypeLayoutInState st1 ty
      coerceValueToLayout layout val0 st1 fs1
  case val.valType of
    TLPointer {} ->
      let fs3 = fs2
            { fsValues = (name, val) : fs2.fsValues
            , fsValuesByName = Map.insert name val fs2.fsValuesByName
            }
      in Right (st2, fs3)
    _ -> emitLocalValue name val st2 fs2

emitVar :: Text -> Maybe Type -> Maybe Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitVar name mType mExpr st fs = do
  (st1, fs1, val0) <- case mExpr of
    Just expr -> emitExpr st fs expr
    Nothing ->
      case mType of
        Nothing -> Left (CompileError "var declaration requires a type or initializer" Nothing Nothing)
        Just ty -> do
          layout <- resolveTypeLayoutInState st ty
          (st1', val) <- emitZeroValue layout st
          Right (st1', fs, val)
  (st2, fs2, val) <- case mType of
    Nothing -> Right (st1, fs1, val0)
    Just ty -> do
      layout <- resolveTypeLayoutInState st1 ty
      coerceValueToLayout layout val0 st1 fs1
  case val.valType of
    TLPointer {} -> Left (CompileError "var cannot have pointer type" Nothing Nothing)
    _ -> emitLocalValue name val st2 fs2

emitLocalValue :: Text -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLocalValue name val st fs = do
  let (baseTy, st1) = emitTypeFromLayout st (val.valType)
  let (ptrTy, st2) = emitPointerType st1 storageClassFunction baseTy
  let (varId, st3) = freshId st2
  let fs1 = addFuncLocal (Instr opVariable [ptrTy, varId, storageClassFunction]) fs
  let fs2 = addFuncInstr (Instr opStore [varId, val.valId]) fs1
  let info = VarInfo (val.valType) varId storageClassFunction ReadWrite []
  let fs3 = fs2
        { fsVars = (name, info) : fs2.fsVars
        , fsVarsByName = Map.insert name info fs2.fsVarsByName
        }
  Right (st3, fs3)

emitIf :: EntryPoint -> [OutputTarget] -> Expr -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitIf entry outTargets cond thenBody elseBody st fs = do
  (st1, fs1, condVal) <- emitExpr st fs cond
  ensureBoolScalar (condVal.valType)
  let (thenLabel, st2) = freshId st1
  let (elseLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3
  let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
  let fs3 = addTerminator (Instr opBranchConditional [condVal.valId, thenLabel, elseLabel]) fs2

  let fsThen0 = addLabel thenLabel fs3
  (st5, fsThen1) <- emitStmtList entry outTargets st4 fsThen0 thenBody
  let thenTerm = fsThen1.fsTerminated
  let fsThen2 = if thenTerm then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

  let fsElse0 = addLabel elseLabel fsThen2
  (st6, fsElse1) <- case elseBody of
    Nothing -> Right (st5, fsElse0)
    Just body -> emitStmtList entry outTargets st5 fsElse0 body
  let elseTerm = fsElse1.fsTerminated
  let fsElse2 = if elseTerm then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

  let fsMerge = addLabel mergeLabel fsElse2
  let fsMerge1 =
        if thenTerm && elseTerm
          then addTerminator (Instr opUnreachable []) fsMerge
          else fsMerge
  Right (st6, fsMerge1)

emitWhile :: EntryPoint -> [OutputTarget] -> Expr -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitWhile entry outTargets cond body st fs = do
  let loopStack = fs.fsLoopStack
  let breakStack = fs.fsBreakStack
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  (st5, fsHeader1, condVal) <- emitExpr st4 fsHeader0 cond
  ensureBoolScalar (condVal.valType)
  let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
  let fsHeader3 = addTerminator (Instr opBranchConditional [condVal.valId, bodyLabel, mergeLabel]) fsHeader2

  let fsBody0 = addLabel bodyLabel fsHeader3
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsBody2) <- emitStmtList entry outTargets st5 fsBody1 body
  let fsBody3 = if fsBody2.fsTerminated then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = addTerminator (Instr opBranch [headerLabel]) (fsContinue0 { fsBreakStack = mergeLabel : breakStack })

  let fsMerge = addLabel mergeLabel fsContinue1
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitLoop :: EntryPoint -> [OutputTarget] -> [Stmt] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLoop entry outTargets body continuing st fs = do
  let loopStack = fs.fsLoopStack
  let breakStack = fs.fsBreakStack
  let (headerLabel, st1) = freshId st
  let (bodyLabel, st2) = freshId st1
  let (continueLabel, st3) = freshId st2
  let (mergeLabel, st4) = freshId st3

  let fs1 = addTerminator (Instr opBranch [headerLabel]) fs
  let fsHeader0 = addLabel headerLabel fs1
  let fsHeader1 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader0
  let fsHeader2 = addTerminator (Instr opBranch [bodyLabel]) fsHeader1

  let fsBody0 = addLabel bodyLabel fsHeader2
  let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st5, fsBody2) <- emitStmtList entry outTargets st4 fsBody1 body
  let fsBody3 = if fsBody2.fsTerminated then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

  let fsContinue0 = addLabel continueLabel fsBody3
  let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
  (st6, fsContinue2) <- case continuing of
    Nothing -> Right (st5, fsContinue1)
    Just contBody -> emitStmtList entry outTargets st5 fsContinue1 contBody
  let fsContinue3 =
        if fsContinue2.fsTerminated
          then fsContinue2
          else addTerminator (Instr opBranch [headerLabel]) fsContinue2

  let fsMerge = addLabel mergeLabel fsContinue3
  let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
  Right (st6, fsMerge1)

emitFor :: EntryPoint -> [OutputTarget] -> Maybe Stmt -> Maybe Expr -> Maybe Stmt -> [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitFor entry outTargets initStmt condExpr contStmt body st fs = do
  (st1, fs1) <- case initStmt of
    Nothing -> Right (st, fs)
    Just s -> emitStmt entry outTargets st fs s
  if fs1.fsTerminated
    then Right (st1, fs1)
    else do
      let loopStack = fs1.fsLoopStack
      let breakStack = fs1.fsBreakStack
      let (headerLabel, st2) = freshId st1
      let (bodyLabel, st3) = freshId st2
      let (continueLabel, st4) = freshId st3
      let (mergeLabel, st5) = freshId st4

      let fs2 = addTerminator (Instr opBranch [headerLabel]) fs1
      let fsHeader0 = addLabel headerLabel fs2
      (st6, fsHeader1, condVal) <- case condExpr of
        Nothing -> do
          let (cid, st') = emitConstBool st5 True
          let (a, sz) = scalarLayout Bool
          let layout = TLScalar Bool a sz
          Right (st', fsHeader0, Value layout cid)
        Just expr -> emitExpr st5 fsHeader0 expr
      ensureBoolScalar (condVal.valType)
      let fsHeader2 = addFuncInstr (Instr opLoopMerge [mergeLabel, continueLabel, loopControlNone]) fsHeader1
      let fsHeader3 = addTerminator (Instr opBranchConditional [condVal.valId, bodyLabel, mergeLabel]) fsHeader2

      let fsBody0 = addLabel bodyLabel fsHeader3
      let fsBody1 = fsBody0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st7, fsBody2) <- emitStmtList entry outTargets st6 fsBody1 body
      let fsBody3 = if fsBody2.fsTerminated then fsBody2 else addTerminator (Instr opBranch [continueLabel]) fsBody2

      let fsContinue0 = addLabel continueLabel fsBody3
      let fsContinue1 = fsContinue0 { fsLoopStack = (mergeLabel, continueLabel) : loopStack, fsBreakStack = mergeLabel : breakStack }
      (st8, fsContinue2) <- case contStmt of
        Nothing -> Right (st7, fsContinue1)
        Just s -> emitStmt entry outTargets st7 fsContinue1 s
      let fsContinue3 =
            if fsContinue2.fsTerminated
              then fsContinue2
              else addTerminator (Instr opBranch [headerLabel]) fsContinue2

      let fsMerge = addLabel mergeLabel fsContinue3
      let fsMerge1 = fsMerge { fsLoopStack = loopStack, fsBreakStack = breakStack }
      Right (st8, fsMerge1)

emitSwitch :: EntryPoint -> [OutputTarget] -> Expr -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitch entry outTargets expr cases defBody st fs = do
  (st1, fs1, selVal) <- emitExpr st fs expr
  ensureSwitchType (selVal.valType)
  cases' <- expandSwitchCases cases defBody
  let breakStack = fs1.fsBreakStack
  let (mergeLabel, st2) = freshId st1
  let fs2 = fs1 { fsBreakStack = mergeLabel : breakStack }
  (st3, fs3) <- emitSwitchChain entry outTargets selVal cases' defBody st2 fs2
  let fs4 = if fs3.fsTerminated then fs3 else addTerminator (Instr opBranch [mergeLabel]) fs3
  let fs5 = addLabel mergeLabel fs4
  let fs6 = fs5 { fsBreakStack = breakStack }
  Right (st3, fs6)

emitSwitchChain :: EntryPoint -> [OutputTarget] -> Value -> [SwitchCase] -> Maybe [Stmt] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitSwitchChain entry outTargets selVal cases defBody st fs =
  case cases of
    [] ->
      case defBody of
        Nothing -> Right (st, fs)
        Just body -> emitStmtList entry outTargets st fs body
    (SwitchCase selectors body : rest) -> do
      (st1, fs1, condVal) <- emitSwitchCond selVal selectors st fs
      ensureBoolScalar (condVal.valType)
      let (thenLabel, st2) = freshId st1
      let (elseLabel, st3) = freshId st2
      let (mergeLabel, st4) = freshId st3
      let fs2 = addFuncInstr (Instr opSelectionMerge [mergeLabel, selectionControlNone]) fs1
      let fs3 = addTerminator (Instr opBranchConditional [condVal.valId, thenLabel, elseLabel]) fs2

      let fsThen0 = addLabel thenLabel fs3
      (st5, fsThen1) <- emitStmtList entry outTargets st4 fsThen0 body
      let fsThen2 = if fsThen1.fsTerminated then fsThen1 else addTerminator (Instr opBranch [mergeLabel]) fsThen1

      let fsElse0 = addLabel elseLabel fsThen2
      (st6, fsElse1) <- emitSwitchChain entry outTargets selVal rest defBody st5 fsElse0
      let fsElse2 = if fsElse1.fsTerminated then fsElse1 else addTerminator (Instr opBranch [mergeLabel]) fsElse1

      let fsMerge = addLabel mergeLabel fsElse2
      let fsMerge1 =
            if fsThen1.fsTerminated && fsElse1.fsTerminated
              then addTerminator (Instr opUnreachable []) fsMerge
              else fsMerge
      Right (st6, fsMerge1)

emitBreak :: GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitBreak st fs =
  case fs.fsBreakStack of
    [] -> Left (CompileError "break used outside of a loop or switch" Nothing Nothing)
    (mergeLabel:_) ->
      let fs1 = addTerminator (Instr opBranch [mergeLabel]) fs
      in Right (st, fs1)

emitContinue :: GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitContinue st fs =
  case fs.fsLoopStack of
    [] -> Left (CompileError "continue used outside of a loop" Nothing Nothing)
    ((_, continueLabel):_) ->
      let fs1 = addTerminator (Instr opBranch [continueLabel]) fs
      in Right (st, fs1)

emitExpr :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState, Value)
emitExpr st fs expr =
  case expr of
    EInt _ n -> do
      (scalar, val) <- selectIntLiteralScalar n
      case scalar of
        I32 -> do
          let (cid, st1) = emitConstI32 st (fromIntegral val)
          let (a, sz) = scalarLayout I32
          let layout = TLScalar I32 a sz
          Right (st1, fs, Value layout cid)
        U32 -> do
          let (cid, st1) = emitConstU32 st (fromIntegral val)
          let (a, sz) = scalarLayout U32
          let layout = TLScalar U32 a sz
          Right (st1, fs, Value layout cid)
        _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
    EFloat _ f -> do
      let (cid, st1) = emitConstF32 st f
      let (a, sz) = scalarLayout F32
      let layout = TLScalar F32 a sz
      Right (st1, fs, Value layout cid)
    EBool _ b -> do
      let (cid, st1) = emitConstBool st b
      let (a, sz) = scalarLayout Bool
      let layout = TLScalar Bool a sz
      Right (st1, fs, Value layout cid)
    EVar _ name ->
      case Map.lookup name fs.fsValuesByName of
        Just val -> Right (st, fs, val)
        Nothing ->
          case Map.lookup name st.gsConstValuesByName of
            Just val -> Right (st, fs, val)
            Nothing ->
              case Map.lookup name (st.gsSamplerLayouts) of
                Just _ | st.gsSamplerMode == SamplerCombined ->
                  Left (CompileError "sampler values are unavailable in combined mode; pass the sampler directly to textureSample" Nothing Nothing)
                _ -> emitLoadFromExpr st fs expr
    EField _ base field -> emitFieldExpr st fs base field
    EIndex _ _ _ -> emitLoadFromExpr st fs expr
    EUnary _ OpNeg inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      case val.valType of
        TLScalar U32 _ _ ->
          case lookupConstKeyById st1 (val.valId) of
            Just key -> do
              key' <- convertConstKey key I32
              let (cid, st2) = emitConstFromKey st1 key'
              let (a, sz) = scalarLayout I32
              let layout = TLScalar I32 a sz
              let (tyId, st3) = emitTypeFromLayout st2 layout
              let (resId, st4) = freshId st3
              let fs2 = addFuncInstr (Instr opSNegate [tyId, resId, cid]) fs1
              Right (st4, fs2, Value layout resId)
            Nothing -> Left (CompileError "unary minus is not supported for u32" Nothing Nothing)
        _ -> do
          opcode <- case classifyNumeric (val.valType) of
            Nothing -> Left (CompileError "unary minus only supports scalar or vector numeric types" Nothing Nothing)
            Just (_, scalar) -> case scalar of
              F32 -> Right opFNegate
              F16 -> Right opFNegate
              I32 -> Right opSNegate
              U32 -> Left (CompileError "unary minus is not supported for u32" Nothing Nothing)
              Bool -> Left (CompileError "unary minus is not supported for bool" Nothing Nothing)
          let (tyId, st2) = emitTypeFromLayout st1 (val.valType)
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opcode [tyId, resId, val.valId]) fs1
          Right (st3, fs2, Value (val.valType) resId)
    EUnary _ OpNot inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      ensureBoolScalar (val.valType)
      let (tyId, st2) = emitTypeFromLayout st1 (val.valType)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opLogicalNot [tyId, resId, val.valId]) fs1
      Right (st3, fs2, Value (val.valType) resId)
    EUnary _ OpAddr inner ->
      case exprToLValue inner of
        Nothing -> Left (CompileError "address-of requires an addressable expression" Nothing Nothing)
        Just lv -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          let layout = TLPointer (ptrInfo.viStorage) (varAccessToPtrAccess (ptrInfo.viAccess)) (ptrInfo.viType)
          Right (st1, fs1, Value layout (ptrInfo.viPtrId))
    EUnary _ OpDeref inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      case val.valType of
        TLPointer _ _ elemLayout -> do
          let (tyId, st2) = emitTypeFromLayout st1 elemLayout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opLoad [tyId, resId, val.valId]) fs1
          Right (st3, fs2, Value elemLayout resId)
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)
    EBinary _ op lhs rhs -> do
      (st1, fs1, lval) <- emitExpr st fs lhs
      (st2, fs2, rval) <- emitExpr st1 fs1 rhs
      case op of
        OpMul ->
          case (lval.valType, rval.valType) of
            (TLMatrix cols rows scalar _ _ _, TLVector n s _ _) -> do
              if n /= cols || s /= scalar
                then Left (CompileError "matrix times vector dimension mismatch" Nothing Nothing)
                else do
                  let (a, sz) = vectorLayout scalar rows
                  let layout = TLVector rows scalar a sz
                  let (tyId, st3) = emitTypeFromLayout st2 layout
                  let (resId, st4) = freshId st3
                  let fs3 = addFuncInstr (Instr opMatrixTimesVector [tyId, resId, lval.valId, rval.valId]) fs2
                  Right (st4, fs3, Value layout resId)
            (TLVector n s _ _, TLMatrix cols rows scalar _ _ _) -> do
              if n /= rows || s /= scalar
                then Left (CompileError "vector times matrix dimension mismatch" Nothing Nothing)
                else do
                  let (a, sz) = vectorLayout scalar cols
                  let layout = TLVector cols scalar a sz
                  let (tyId, st3) = emitTypeFromLayout st2 layout
                  let (resId, st4) = freshId st3
                  let fs3 = addFuncInstr (Instr opVectorTimesMatrix [tyId, resId, lval.valId, rval.valId]) fs2
                  Right (st4, fs3, Value layout resId)
            (TLMatrix colsA rowsA scalarA _ _ _, TLMatrix colsB rowsB scalarB _ _ _) -> do
              if colsA /= rowsB || scalarA /= scalarB
                then Left (CompileError "matrix times matrix dimension mismatch" Nothing Nothing)
                else do
                  let layout = matrixLayout colsB rowsA scalarA
                  let (tyId, st3) = emitTypeFromLayout st2 layout
                  let (resId, st4) = freshId st3
                  let fs3 = addFuncInstr (Instr opMatrixTimesMatrix [tyId, resId, lval.valId, rval.valId]) fs2
                  Right (st4, fs3, Value layout resId)
            _ -> do
              (st3, fs3, lval', rval', layout) <- coerceBinaryOperands lval rval st2 fs2
              emitBinary op layout (lval'.valId) (rval'.valId) st3 fs3
        _ -> do
          (st3, fs3, lval', rval', layout) <- coerceBinaryOperands lval rval st2 fs2
          emitBinary op layout (lval'.valId) (rval'.valId) st3 fs3
    ECall _ name args -> emitCall name args st fs
    EBitcast _ targetTy inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      targetLayout <- resolveBitcastLayout targetTy
      (srcN, srcSz, _) <- bitcastLayoutInfo (val.valType)
      (dstN, dstSz, _) <- bitcastLayoutInfo targetLayout
      when (srcN /= dstN || srcSz /= dstSz) $
        Left (CompileError "bitcast source and target types must have the same size" Nothing Nothing)
      if val.valType == targetLayout
        then Right (st1, fs1, val)
        else emitBitcastValue targetLayout val st1 fs1

emitLoadFromExpr :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState, Value)
emitLoadFromExpr st fs expr =
  case exprToLValue expr of
    Nothing -> Left (CompileError "expected addressable expression" Nothing Nothing)
    Just lv -> do
      (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
      case ptrInfo.viType of
        TLAtomic _ -> Left (CompileError "use atomicLoad for atomic values" Nothing Nothing)
        _ -> emitLoadFromPtr st1 fs1 ptrInfo

emitFieldExpr :: GenState -> FuncState -> Expr -> Text -> Either CompileError (GenState, FuncState, Value)
emitFieldExpr st fs base field = do
  (st1, fs1, baseVal) <- emitExpr st fs base
  case baseVal.valType of
    TLVector n scalar _ _ -> do
      idxs <- vectorFieldIndices field n
      case idxs of
        [ix] -> do
          let (a, sz) = scalarLayout scalar
          let layout = TLScalar scalar a sz
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opCompositeExtract [tyId, resId, baseVal.valId, ix]) fs1
          Right (st3, fs2, Value layout resId)
        _ -> do
          let len = length idxs
          let (a, sz) = vectorLayout scalar len
          let layout = TLVector len scalar a sz
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let shuffleOps = [tyId, resId, baseVal.valId, baseVal.valId] <> idxs
          let fs2 = addFuncInstr (Instr opVectorShuffle shuffleOps) fs1
          Right (st3, fs2, Value layout resId)
    TLStruct _ fields _ _ -> do
      (ix, fieldLayout) <- findField (textToString field) fields
      let (tyId, st2) = emitTypeFromLayout st1 fieldLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opCompositeExtract [tyId, resId, baseVal.valId, fromIntegral ix]) fs1
      Right (st3, fs2, Value fieldLayout resId)
    _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)

emitBinary :: BinOp -> TypeLayout -> Word32 -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBinary op layout lhs rhs st fs =
  case op of
    OpAdd -> emitArith
    OpSub -> emitArith
    OpMul -> emitArith
    OpDiv -> emitArith
    OpMod -> emitMod
    OpAnd -> emitLogical
    OpOr -> emitLogical
    OpBitAnd -> emitBitwise
    OpBitOr -> emitBitwise
    OpBitXor -> emitBitwise
    OpShl -> emitShift
    OpShr -> emitShift
    OpEq -> emitCompare
    OpNe -> emitCompare
    OpLt -> emitCompare
    OpLe -> emitCompare
    OpGt -> emitCompare
    OpGe -> emitCompare
  where
    emitArith = do
      case classifyNumeric layout of
        Nothing -> Left (CompileError "binary operators only support scalar or vector numeric types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          Bool -> Left (CompileError "arithmetic operators do not support bool" Nothing Nothing)
          _ -> do
            let (tyId, st1) = emitTypeFromLayout st layout
            let (resId, st2) = freshId st1
            let opcode = case (scalar, op) of
                  (F32, OpAdd) -> opFAdd
                  (F16, OpAdd) -> opFAdd
                  (F32, OpSub) -> opFSub
                  (F16, OpSub) -> opFSub
                  (F32, OpMul) -> opFMul
                  (F16, OpMul) -> opFMul
                  (F32, OpDiv) -> opFDiv
                  (F16, OpDiv) -> opFDiv
                  (I32, OpAdd) -> opIAdd
                  (U32, OpAdd) -> opIAdd
                  (I32, OpSub) -> opISub
                  (U32, OpSub) -> opISub
                  (I32, OpMul) -> opIMul
                  (U32, OpMul) -> opIMul
                  (I32, OpDiv) -> opSDiv
                  (U32, OpDiv) -> opUDiv
                  _ -> opIAdd
            let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
            Right (st2, fs1, Value layout resId)
    emitMod =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "modulo only supports scalar or vector integer types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          I32 -> emitIntOp opSRem
          U32 -> emitIntOp opUMod
          _ -> Left (CompileError "modulo only supports i32 or u32 types" Nothing Nothing)
    emitLogical =
      case layout of
        TLScalar Bool _ _ -> do
          let (tyId, st1) = emitTypeFromLayout st layout
          let (resId, st2) = freshId st1
          let opcode = case op of
                OpAnd -> opLogicalAnd
                OpOr -> opLogicalOr
                _ -> opLogicalAnd
          let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
          Right (st2, fs1, Value layout resId)
        _ -> Left (CompileError "logical operators require bool scalars" Nothing Nothing)
    emitBitwise =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "bitwise operators require i32 or u32 scalar or vector types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          I32 -> emitIntOp opcode
          U32 -> emitIntOp opcode
          _ -> Left (CompileError "bitwise operators require i32 or u32 types" Nothing Nothing)
      where
        opcode = case op of
          OpBitAnd -> opBitwiseAnd
          OpBitOr -> opBitwiseOr
          OpBitXor -> opBitwiseXor
          _ -> opBitwiseAnd
    emitShift =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "shift operators require i32 or u32 scalar or vector types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          I32 -> emitIntOp shiftOp
          U32 -> emitIntOp shiftOp
          _ -> Left (CompileError "shift operators require i32 or u32 types" Nothing Nothing)
      where
        shiftOp = case op of
          OpShl -> opShiftLeftLogical
          OpShr ->
            case classifyNumeric layout of
              Just (_, I32) -> opShiftRightArithmetic
              Just (_, U32) -> opShiftRightLogical
              _ -> opShiftRightLogical
          _ -> opShiftLeftLogical
    emitIntOp opcode = do
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
      Right (st2, fs1, Value layout resId)
    emitCompare =
      case classifyNumeric layout of
        Nothing -> Left (CompileError "comparison operators only support scalar or vector numeric types" Nothing Nothing)
        Just (n, scalar) -> do
          let resultLayout = boolResultLayout n
          let (tyId, st1) = emitTypeFromLayout st resultLayout
          let (resId, st2) = freshId st1
          opcode <- case scalar of
            Bool ->
              if n /= 1
                then Left (CompileError "bool vector comparisons are not supported" Nothing Nothing)
                else case op of
                  OpEq -> Right opLogicalEqual
                  OpNe -> Right opLogicalNotEqual
                  _ -> Left (CompileError "ordered comparisons are not supported for bool" Nothing Nothing)
            F32 ->
              case op of
                OpEq -> Right opFOrdEqual
                OpNe -> Right opFOrdNotEqual
                OpLt -> Right opFOrdLessThan
                OpLe -> Right opFOrdLessThanEqual
                OpGt -> Right opFOrdGreaterThan
                OpGe -> Right opFOrdGreaterThanEqual
                _ -> Left (CompileError "unsupported float comparison" Nothing Nothing)
            F16 ->
              case op of
                OpEq -> Right opFOrdEqual
                OpNe -> Right opFOrdNotEqual
                OpLt -> Right opFOrdLessThan
                OpLe -> Right opFOrdLessThanEqual
                OpGt -> Right opFOrdGreaterThan
                OpGe -> Right opFOrdGreaterThanEqual
                _ -> Left (CompileError "unsupported float comparison" Nothing Nothing)
            I32 ->
              case op of
                OpEq -> Right opIEqual
                OpNe -> Right opINotEqual
                OpLt -> Right opSLessThan
                OpLe -> Right opSLessThanEqual
                OpGt -> Right opSGreaterThan
                OpGe -> Right opSGreaterThanEqual
                _ -> Left (CompileError "unsupported int comparison" Nothing Nothing)
            U32 ->
              case op of
                OpEq -> Right opIEqual
                OpNe -> Right opINotEqual
                OpLt -> Right opULessThan
                OpLe -> Right opULessThanEqual
                OpGt -> Right opUGreaterThan
                OpGe -> Right opUGreaterThanEqual
                _ -> Left (CompileError "unsupported uint comparison" Nothing Nothing)
          let fs1 = addFuncInstr (Instr opcode [tyId, resId, lhs, rhs]) fs
          Right (st2, fs1, Value resultLayout resId)

emitCall :: Text -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCall name args st fs =
  case parseVectorCtorName name of
    Just (n, targetScalar) -> emitVectorCtor n targetScalar args st fs
    Nothing ->
      case parseArrayCtorName name of
        Just (elemTy, arrLen) -> emitTypedArrayCtor elemTy arrLen args st fs
        Nothing ->
          case name of
            "vec2" -> emitVectorCtor 2 Nothing args st fs
            "vec3" -> emitVectorCtor 3 Nothing args st fs
            "vec4" -> emitVectorCtor 4 Nothing args st fs
            "array" -> emitArrayCtor args st fs
            "f16" -> emitScalarCtor F16 args st fs
            "f32" -> emitScalarCtor F32 args st fs
            "u32" -> emitScalarCtor U32 args st fs
            "i32" -> emitScalarCtor I32 args st fs
            "abs" -> emitAbsBuiltin args st fs
            "min" -> emitMinMaxBuiltin True args st fs
            "max" -> emitMinMaxBuiltin False args st fs
            "clamp" -> emitClampBuiltin args st fs
            "mix" -> emitGLSLTrinary glslStd450FMix args st fs
            "select" -> emitSelectBuiltin args st fs
            "any" -> emitAnyAll opAny args st fs
            "all" -> emitAnyAll opAll args st fs
            "round" -> emitGLSLUnary glslStd450Round args st fs
            "roundEven" -> emitGLSLUnary glslStd450RoundEven args st fs
            "trunc" -> emitGLSLUnary glslStd450Trunc args st fs
            "step" -> emitGLSLBinary glslStd450Step args st fs
            "smoothstep" -> emitGLSLTrinary glslStd450SmoothStep args st fs
            "floor" -> emitGLSLUnary glslStd450Floor args st fs
            "ceil" -> emitGLSLUnary glslStd450Ceil args st fs
            "fract" -> emitGLSLUnary glslStd450Fract args st fs
            "radians" -> emitGLSLUnary glslStd450Radians args st fs
            "degrees" -> emitGLSLUnary glslStd450Degrees args st fs
            "exp" -> emitGLSLUnary glslStd450Exp args st fs
            "log" -> emitGLSLUnary glslStd450Log args st fs
            "exp2" -> emitGLSLUnary glslStd450Exp2 args st fs
            "log2" -> emitGLSLUnary glslStd450Log2 args st fs
            "sin" -> emitGLSLUnary glslStd450Sin args st fs
            "cos" -> emitGLSLUnary glslStd450Cos args st fs
            "tan" -> emitGLSLUnary glslStd450Tan args st fs
            "asin" -> emitGLSLUnary glslStd450Asin args st fs
            "acos" -> emitGLSLUnary glslStd450Acos args st fs
            "atan" -> emitGLSLUnary glslStd450Atan args st fs
            "atan2" -> emitGLSLBinary glslStd450Atan2 args st fs
            "sinh" -> emitGLSLUnary glslStd450Sinh args st fs
            "cosh" -> emitGLSLUnary glslStd450Cosh args st fs
            "tanh" -> emitGLSLUnary glslStd450Tanh args st fs
            "asinh" -> emitGLSLUnary glslStd450Asinh args st fs
            "acosh" -> emitGLSLUnary glslStd450Acosh args st fs
            "atanh" -> emitGLSLUnary glslStd450Atanh args st fs
            "pow" -> emitGLSLBinary glslStd450Pow args st fs
            "sqrt" -> emitGLSLUnary glslStd450Sqrt args st fs
            "inverseSqrt" -> emitGLSLUnary glslStd450InverseSqrt args st fs
            "fma" -> emitGLSLTrinary glslStd450Fma args st fs
            "sign" -> emitSignBuiltin args st fs
            "length" -> emitGLSLLength args st fs
            "normalize" -> emitGLSLUnary glslStd450Normalize args st fs
            "dot" -> emitDot args st fs
            "cross" -> emitCrossBuiltin args st fs
            "distance" -> emitDistance args st fs
            "faceForward" -> emitFaceForwardBuiltin args st fs
            "reflect" -> emitReflect args st fs
            "refract" -> emitRefractBuiltin args st fs
            "transpose" -> emitTransposeBuiltin args st fs
            "determinant" -> emitDeterminantBuiltin args st fs
            "inverse" -> emitMatrixInverseBuiltin args st fs
            "modf" -> emitModfBuiltin args st fs
            "frexp" -> emitFrexpBuiltin args st fs
            "ldexp" -> emitLdexpBuiltin args st fs
            "pack4x8snorm" -> emitPackBuiltin glslStd450PackSnorm4x8 4 args st fs
            "pack4x8unorm" -> emitPackBuiltin glslStd450PackUnorm4x8 4 args st fs
            "pack2x16snorm" -> emitPackBuiltin glslStd450PackSnorm2x16 2 args st fs
            "pack2x16unorm" -> emitPackBuiltin glslStd450PackUnorm2x16 2 args st fs
            "pack2x16float" -> emitPackBuiltin glslStd450PackHalf2x16 2 args st fs
            "unpack4x8snorm" -> emitUnpackBuiltin glslStd450UnpackSnorm4x8 4 args st fs
            "unpack4x8unorm" -> emitUnpackBuiltin glslStd450UnpackUnorm4x8 4 args st fs
            "unpack2x16snorm" -> emitUnpackBuiltin glslStd450UnpackSnorm2x16 2 args st fs
            "unpack2x16unorm" -> emitUnpackBuiltin glslStd450UnpackUnorm2x16 2 args st fs
            "unpack2x16float" -> emitUnpackBuiltin glslStd450UnpackHalf2x16 2 args st fs
            "firstLeadingBit" -> emitFirstLeadingBitBuiltin args st fs
            "firstTrailingBit" -> emitFirstTrailingBitBuiltin args st fs
            "saturate" -> emitSaturateBuiltin args st fs
            "quantizeToF16" -> emitQuantizeToF16 args st fs
            "countOneBits" -> emitBitUnary opBitCount args st fs
            "countLeadingZeros" -> emitCountLeadingZeros args st fs
            "countTrailingZeros" -> emitCountTrailingZeros args st fs
            "reverseBits" -> emitBitUnary opBitReverse args st fs
            "extractBits" -> emitExtractBitsBuiltin args st fs
            "insertBits" -> emitInsertBitsBuiltin args st fs
            "dot4U8Packed" -> emitDot4Packed False args st fs
            "dot4I8Packed" -> emitDot4Packed True args st fs
            "arrayLength" -> emitArrayLengthBuiltin args st fs
            "textureSample" -> emitTextureSample args st fs
            "textureSampleCompare" -> emitTextureSampleCompare args st fs
            "textureSampleLevel" -> emitTextureSampleLevel args st fs
            "textureSampleBias" -> emitTextureSampleBias args st fs
            "textureSampleGrad" -> emitTextureSampleGrad args st fs
            "textureSampleCompareLevel" -> emitTextureSampleCompareLevel args st fs
            "textureGather" -> emitTextureGather args st fs
            "textureGatherCompare" -> emitTextureGatherCompare args st fs
            "textureDimensions" -> emitTextureDimensions args st fs
            "textureNumLevels" -> emitTextureNumLevels args st fs
            "textureNumLayers" -> emitTextureNumLayers args st fs
            "textureNumSamples" -> emitTextureNumSamples args st fs
            "dpdx" -> emitDerivative opDPdx args st fs
            "dpdy" -> emitDerivative opDPdy args st fs
            "fwidth" -> emitDerivative opFwidth args st fs
            "workgroupBarrier" -> Left (CompileError "workgroupBarrier cannot be used as a value" Nothing Nothing)
            "storageBarrier" -> Left (CompileError "storageBarrier cannot be used as a value" Nothing Nothing)
            "textureBarrier" -> Left (CompileError "textureBarrier cannot be used as a value" Nothing Nothing)
            "textureLoad" -> emitTextureLoad args st fs
            "atomicLoad" -> emitAtomicLoad args st fs
            "atomicAdd" -> emitAtomicBinary (atomicOpSame "atomicAdd" opAtomicIAdd) args st fs
            "atomicSub" -> emitAtomicBinary (atomicOpSame "atomicSub" opAtomicISub) args st fs
            "atomicMin" -> emitAtomicBinary (atomicOpSignedUnsigned "atomicMin" opAtomicSMin opAtomicUMin) args st fs
            "atomicMax" -> emitAtomicBinary (atomicOpSignedUnsigned "atomicMax" opAtomicSMax opAtomicUMax) args st fs
            "atomicAnd" -> emitAtomicBinary (atomicOpSame "atomicAnd" opAtomicAnd) args st fs
            "atomicOr" -> emitAtomicBinary (atomicOpSame "atomicOr" opAtomicOr) args st fs
            "atomicXor" -> emitAtomicBinary (atomicOpSame "atomicXor" opAtomicXor) args st fs
            "atomicExchange" -> emitAtomicBinary (atomicOpSame "atomicExchange" opAtomicExchange) args st fs
            "atomicCompareExchangeWeak" -> emitAtomicCompareExchangeWeak args st fs
            _ ->
              case parseMatrixCtorName name of
                Just (cols, rows, targetScalar) -> emitMatrixCtor cols rows targetScalar args st fs
                Nothing ->
                  maybe
                    (do
                        (st1, fs1, mval) <- emitFunctionCallByName name args st fs
                        maybe
                          (Left (CompileError "void function call cannot be used as a value" Nothing Nothing))
                          (\val -> Right (st1, fs1, val))
                          mval
                    )
                    (\layout -> emitStructCtor name layout args st fs)
                    (lookup name (st.gsStructLayouts))

atomicOpSame :: Text -> Word16 -> Scalar -> Either CompileError Word16
atomicOpSame label op scalar =
  case scalar of
    I32 -> Right op
    U32 -> Right op
    _ -> Left (CompileError (textToString label <> " requires i32 or u32") Nothing Nothing)

atomicOpSignedUnsigned :: Text -> Word16 -> Word16 -> Scalar -> Either CompileError Word16
atomicOpSignedUnsigned label opSigned opUnsigned scalar =
  case scalar of
    I32 -> Right opSigned
    U32 -> Right opUnsigned
    _ -> Left (CompileError (textToString label <> " requires i32 or u32") Nothing Nothing)

emitGLSLUnary :: Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLUnary inst args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (st2, fs2, val') <- case val.valType of
        TLScalar F32 _ _ -> Right (st1, fs1, val)
        TLScalar F16 _ _ -> Right (st1, fs1, val)
        TLScalar {} -> do
          let (a, sz) = scalarLayout F32
          let layout = TLScalar F32 a sz
          coerceValueToLayout layout val st1 fs1
        _ -> Right (st1, fs1, val)
      ensureFloatNumeric (val'.valType)
      emitExtInst (val'.valType) inst [val'.valId] st2 fs2
    _ -> Left (CompileError "builtin expects one argument" Nothing Nothing)

emitGLSLBinary :: Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLBinary inst args st fs =
  case args of
    [a, b] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      (st3, fs3, v1', v2', layout) <-
        if v1.valType == v2.valType
          then Right (st2, fs2, v1, v2, v1.valType)
          else coerceBinaryOperands v1 v2 st2 fs2
      ensureFloatNumeric layout
      emitExtInst layout inst [v1'.valId, v2'.valId] st3 fs3
    _ -> Left (CompileError "builtin expects two arguments" Nothing Nothing)

emitGLSLTrinary :: Word32 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLTrinary inst args st fs =
  case args of
    [a, b, c] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      (st3, fs3, v3) <- emitExpr st2 fs2 c
      let targetLayout = pickBaseLayout st3 [v1, v2, v3]
      (st4, fs4, v1') <- coerceValueToLayout targetLayout v1 st3 fs3
      (st5, fs5, v2') <- coerceValueToLayout targetLayout v2 st4 fs4
      (st6, fs6, v3') <- coerceValueToLayout targetLayout v3 st5 fs5
      ensureFloatNumeric targetLayout
      emitExtInst targetLayout inst [v1'.valId, v2'.valId, v3'.valId] st6 fs6
    _ -> Left (CompileError "builtin expects three arguments" Nothing Nothing)

emitGLSLLength :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitGLSLLength args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatVector (val.valType)
      scalar <- case val.valType of
        TLVector _ s _ _ -> Right s
        _ -> Left (CompileError "length expects a float vector" Nothing Nothing)
      let (align, size) = scalarLayout scalar
      let layout = TLScalar scalar align size
      emitExtInst layout glslStd450Length [val.valId] st1 fs1
    _ -> Left (CompileError "length expects one argument" Nothing Nothing)

emitDot :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDot args st fs =
  case args of
    [a, b] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      ensureTypeMatch (v1.valType) (v2.valType)
      ensureFloatVector (v1.valType)
      scalar <- case v1.valType of
        TLVector _ s _ _ -> Right s
        _ -> Left (CompileError "dot expects float vectors" Nothing Nothing)
      let (align, size) = scalarLayout scalar
      let layout = TLScalar scalar align size
      let (tyId, st3) = emitTypeFromLayout st2 layout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opDot [tyId, resId, v1.valId, v2.valId]) fs2
      Right (st4, fs3, Value layout resId)
    _ -> Left (CompileError "dot expects two arguments" Nothing Nothing)

emitDistance :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDistance args st fs =
  case args of
    [a, b] -> do
      (st1, fs1, v1) <- emitExpr st fs a
      (st2, fs2, v2) <- emitExpr st1 fs1 b
      (st3, fs3, v1', v2', layout) <-
        if v1.valType == v2.valType
          then Right (st2, fs2, v1, v2, v1.valType)
          else coerceBinaryOperands v1 v2 st2 fs2
      ensureFloatNumeric layout
      case layout of
        TLScalar {} -> do
          (st4, fs4, diff) <- emitBinary OpSub layout (v1'.valId) (v2'.valId) st3 fs3
          emitExtInst (diff.valType) glslStd450FAbs [diff.valId] st4 fs4
        TLVector _ scalar _ _ -> do
          (st4, fs4, diff) <- emitBinary OpSub layout (v1'.valId) (v2'.valId) st3 fs3
          let (a', sz') = scalarLayout scalar
          let distLayout = TLScalar scalar a' sz'
          emitExtInst distLayout glslStd450Length [diff.valId] st4 fs4
        _ -> Left (CompileError "distance expects float scalar or vector arguments" Nothing Nothing)
    _ -> Left (CompileError "distance expects two arguments" Nothing Nothing)

emitReflect :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitReflect args st fs =
  case args of
    [iExpr, nExpr] -> do
      (st1, fs1, iVal) <- emitExpr st fs iExpr
      (st2, fs2, nVal) <- emitExpr st1 fs1 nExpr
      ensureTypeMatch (iVal.valType) (nVal.valType)
      ensureFloatVector (iVal.valType)
      scalar <- case iVal.valType of
        TLVector _ s _ _ -> Right s
        _ -> Left (CompileError "reflect expects float vector arguments" Nothing Nothing)
      let (a', sz') = scalarLayout scalar
      let dotLayout = TLScalar scalar a' sz'
      let (dotTy, st3) = emitTypeFromLayout st2 dotLayout
      let (dotId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opDot [dotTy, dotId, nVal.valId, iVal.valId]) fs2
      (twoId, st5) <- emitConstFloatScalar scalar 2.0 st4
      (st6, fs4, scaleVal) <- emitBinary OpMul dotLayout dotId twoId st5 fs3
      (st7, fs5, scaleVec) <- emitSplatVector (iVal.valType) (scaleVal.valId) st6 fs4
      (st8, fs6, scaledN) <- emitBinary OpMul (nVal.valType) (nVal.valId) (scaleVec.valId) st7 fs5
      emitBinary OpSub (iVal.valType) (iVal.valId) (scaledN.valId) st8 fs6
    _ -> Left (CompileError "reflect expects two arguments" Nothing Nothing)

emitSelectValue :: TypeLayout -> Value -> Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSelectValue layout trueVal falseVal condVal st fs = do
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opSelect [tyId, resId, condVal.valId, trueVal.valId, falseVal.valId]) fs
  Right (st2, fs1, Value layout resId)

emitSelectBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSelectBuiltin args st fs =
  case args of
    [aExpr, bExpr, condExpr] -> do
      (st1, fs1, aVal) <- emitExpr st fs aExpr
      (st2, fs2, bVal) <- emitExpr st1 fs1 bExpr
      (st3, fs3, condVal) <- emitExpr st2 fs2 condExpr
      (st4, fs4, aVal', bVal', layout) <- coerceBinaryOperands aVal bVal st3 fs3
      case layout of
        TLScalar {} -> ensureBoolScalar (condVal.valType)
        TLVector n _ _ _ -> ensureBoolVectorSize n (condVal.valType)
        _ -> Left (CompileError "select expects scalar or vector types" Nothing Nothing)
      emitSelectValue layout bVal' aVal' condVal st4 fs4
    _ -> Left (CompileError "select expects (a, b, cond)" Nothing Nothing)

emitAnyAll :: Word16 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAnyAll opcode args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case val.valType of
        TLScalar Bool _ _ -> Right (st1, fs1, val)
        TLVector _ Bool _ _ -> do
          let (a, sz) = scalarLayout Bool
          let layout = TLScalar Bool a sz
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opcode [tyId, resId, val.valId]) fs1
          Right (st3, fs2, Value layout resId)
        _ -> Left (CompileError "any/all expect a bool vector" Nothing Nothing)
    _ -> Left (CompileError "any/all expect one argument" Nothing Nothing)

layoutSuffix :: TypeLayout -> Either CompileError String
layoutSuffix layout =
  case layout of
    TLScalar s _ _ -> Right (scalarSuffix s)
    TLVector n s _ _ -> Right ("v" <> show n <> "_" <> scalarSuffix s)
    TLMatrix c r s _ _ _ -> Right ("m" <> show c <> "x" <> show r <> "_" <> scalarSuffix s)
    _ -> Left (CompileError "unsupported layout for builtin struct" Nothing Nothing)
  where
    scalarSuffix s = case s of
      I32 -> "i32"
      U32 -> "u32"
      F16 -> "f16"
      F32 -> "f32"
      Bool -> "bool"

makeBuiltinStructLayout :: String -> [(String, TypeLayout)] -> TypeLayout
makeBuiltinStructLayout name fields =
  let (fields', align) = buildFields 0 [] 1 fields
      size = structSize fields' align
  in TLStruct name fields' align size
  where
    buildFields _ acc alignAcc [] = (reverse acc, alignAcc)
    buildFields offset acc alignAcc ((fname, fLayout):rest) =
      let fAlign = layoutAlign fLayout
          fSize = layoutSize fLayout
          aligned = roundUp offset fAlign
          entry = FieldLayout fname aligned fLayout fAlign fSize
          alignAcc' = max alignAcc fAlign
      in buildFields (aligned + fSize) (entry:acc) alignAcc' rest

modfStructLayout :: TypeLayout -> Either CompileError TypeLayout
modfStructLayout baseLayout = do
  suffix <- layoutSuffix baseLayout
  let name = "__modf_result_" <> suffix
  pure (makeBuiltinStructLayout name [("fract", baseLayout), ("whole", baseLayout)])

frexpStructLayout :: TypeLayout -> Either CompileError TypeLayout
frexpStructLayout baseLayout = do
  suffix <- layoutSuffix baseLayout
  expLayout <- case baseLayout of
    TLScalar {} ->
      let (a, sz) = scalarLayout I32
      in Right (TLScalar I32 a sz)
    TLVector n _ _ _ ->
      let (a, sz) = vectorLayout I32 n
      in Right (TLVector n I32 a sz)
    _ -> Left (CompileError "frexp expects float scalar or vector" Nothing Nothing)
  let name = "__frexp_result_" <> suffix
  pure (makeBuiltinStructLayout name [("fract", baseLayout), ("exp", expLayout)])

emitFloatConvert :: Scalar -> Scalar -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFloatConvert fromScalar toScalarTy val st fs =
  case val.valType of
    TLScalar s _ _ | s == fromScalar -> emitScalarConvert fromScalar toScalarTy val st fs
    TLVector n s _ _ | s == fromScalar -> do
      let (a, sz) = vectorLayout toScalarTy n
      let layout = TLVector n toScalarTy a sz
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opFConvert [tyId, resId, val.valId]) fs
      Right (st2, fs1, Value layout resId)
    _ -> Left (CompileError "expected float scalar or vector for conversion" Nothing Nothing)

emitQuantizeToF16 :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitQuantizeToF16 args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case val.valType of
        TLScalar F32 _ _ -> do
          (st2, fs2, halfVal) <- emitFloatConvert F32 F16 val st1 fs1
          emitFloatConvert F16 F32 halfVal st2 fs2
        TLVector _ F32 _ _ -> do
          (st2, fs2, halfVal) <- emitFloatConvert F32 F16 val st1 fs1
          emitFloatConvert F16 F32 halfVal st2 fs2
        _ -> Left (CompileError "quantizeToF16 expects f32 scalar or vector" Nothing Nothing)
    _ -> Left (CompileError "quantizeToF16 expects one argument" Nothing Nothing)

emitModfBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitModfBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (val.valType)
      layout <- modfStructLayout (val.valType)
      emitExtInst layout glslStd450ModfStruct [val.valId] st1 fs1
    _ -> Left (CompileError "modf expects one argument" Nothing Nothing)

emitFrexpBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFrexpBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (val.valType)
      layout <- frexpStructLayout (val.valType)
      emitExtInst layout glslStd450FrexpStruct [val.valId] st1 fs1
    _ -> Left (CompileError "frexp expects one argument" Nothing Nothing)

emitLdexpBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitLdexpBuiltin args st fs =
  case args of
    [xExpr, expExpr] -> do
      (st1, fs1, xVal) <- emitExpr st fs xExpr
      ensureFloatNumeric (xVal.valType)
      (st2, fs2, expVal) <- emitExpr st1 fs1 expExpr
      (st3, fs3, expVal') <- coerceExpToI32 (xVal.valType) expVal st2 fs2
      emitExtInst (xVal.valType) glslStd450Ldexp [xVal.valId, expVal'.valId] st3 fs3
    _ -> Left (CompileError "ldexp expects (x, exp)" Nothing Nothing)

emitPackBuiltin :: Word32 -> Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitPackBuiltin inst n args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatVecN n (val.valType)
      let (a, sz) = scalarLayout U32
      let layout = TLScalar U32 a sz
      emitExtInst layout inst [val.valId] st1 fs1
    _ -> Left (CompileError "pack builtin expects one argument" Nothing Nothing)

emitUnpackBuiltin :: Word32 -> Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitUnpackBuiltin inst n args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      let (a, sz) = scalarLayout U32
      let layoutU32 = TLScalar U32 a sz
      (st2, fs2, val') <- coerceValueToLayout layoutU32 val st1 fs1
      let (va, vsz) = vectorLayout F32 n
      let layout = TLVector n F32 va vsz
      emitExtInst layout inst [val'.valId] st2 fs2
    _ -> Left (CompileError "unpack builtin expects one argument" Nothing Nothing)

emitTransposeBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTransposeBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (cols, rows, scalar) <- ensureFloatMatrix (val.valType)
      let layout = matrixLayout rows cols scalar
      let (tyId, st2) = emitTypeFromLayout st1 layout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opTranspose [tyId, resId, val.valId]) fs1
      Right (st3, fs2, Value layout resId)
    _ -> Left (CompileError "transpose expects one argument" Nothing Nothing)

emitDeterminantBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDeterminantBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (cols, rows, scalar) <- ensureFloatMatrix (val.valType)
      when (cols /= rows) $
        Left (CompileError "determinant expects a square matrix" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let layout = TLScalar scalar a sz
      emitExtInst layout glslStd450Determinant [val.valId] st1 fs1
    _ -> Left (CompileError "determinant expects one argument" Nothing Nothing)

emitMatrixInverseBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMatrixInverseBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (cols, rows, _) <- ensureFloatMatrix (val.valType)
      when (cols /= rows) $
        Left (CompileError "inverse expects a square matrix" Nothing Nothing)
      emitExtInst (val.valType) glslStd450MatrixInverse [val.valId] st1 fs1
    _ -> Left (CompileError "inverse expects one argument" Nothing Nothing)

emitCrossBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCrossBuiltin args st fs =
  case args of
    [aExpr, bExpr] -> do
      (st1, fs1, v1) <- emitExpr st fs aExpr
      (st2, fs2, v2) <- emitExpr st1 fs1 bExpr
      (st3, fs3, v1', v2', layout) <-
        if v1.valType == v2.valType
          then Right (st2, fs2, v1, v2, v1.valType)
          else coerceBinaryOperands v1 v2 st2 fs2
      ensureFloatVec3 layout
      emitExtInst layout glslStd450Cross [v1'.valId, v2'.valId] st3 fs3
    _ -> Left (CompileError "cross expects two arguments" Nothing Nothing)

emitFaceForwardBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFaceForwardBuiltin args st fs =
  case args of
    [nExpr, iExpr, nrefExpr] -> do
      (st1, fs1, nVal) <- emitExpr st fs nExpr
      (st2, fs2, iVal) <- emitExpr st1 fs1 iExpr
      (st3, fs3, nrefVal) <- emitExpr st2 fs2 nrefExpr
      (st4, fs4, nVal', iVal', layout) <-
        if nVal.valType == iVal.valType
          then Right (st3, fs3, nVal, iVal, nVal.valType)
          else coerceBinaryOperands nVal iVal st3 fs3
      (st5, fs5, nrefVal') <- coerceValueToLayout layout nrefVal st4 fs4
      ensureFloatVector layout
      emitExtInst layout glslStd450FaceForward [nVal'.valId, iVal'.valId, nrefVal'.valId] st5 fs5
    _ -> Left (CompileError "faceForward expects three arguments" Nothing Nothing)

emitRefractBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitRefractBuiltin args st fs =
  case args of
    [iExpr, nExpr, etaExpr] -> do
      (st1, fs1, iVal) <- emitExpr st fs iExpr
      (st2, fs2, nVal) <- emitExpr st1 fs1 nExpr
      (st3, fs3, etaVal) <- emitExpr st2 fs2 etaExpr
      (st4, fs4, iVal', nVal', layout) <-
        if iVal.valType == nVal.valType
          then Right (st3, fs3, iVal, nVal, iVal.valType)
          else coerceBinaryOperands iVal nVal st3 fs3
      ensureFloatVector layout
      (st5, fs5, etaScalar) <- case etaVal.valType of
        TLScalar s _ _ ->
          if s == scalarFromLayout layout
            then Right (st4, fs4, etaVal)
            else do
              (st5, fs5, etaConverted) <- emitScalarConvert s (scalarFromLayout layout) etaVal st4 fs4
              Right (st5, fs5, etaConverted)
        _ -> Left (CompileError "refract expects a float scalar for eta" Nothing Nothing)
      emitExtInst layout glslStd450Refract [iVal'.valId, nVal'.valId, etaScalar.valId] st5 fs5
    _ -> Left (CompileError "refract expects three arguments" Nothing Nothing)
  where
    scalarFromLayout layout =
      case layout of
        TLVector _ s _ _ -> s
        _ -> F32

emitFirstTrailingBitBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFirstTrailingBitBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureIntNumeric (val.valType)
      emitExtInst (val.valType) glslStd450FindILsb [val.valId] st1 fs1
    _ -> Left (CompileError "firstTrailingBit expects one argument" Nothing Nothing)

emitFirstLeadingBitBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitFirstLeadingBitBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureIntNumeric (val.valType)
      let inst = case classifyNumeric (val.valType) of
            Just (_, I32) -> glslStd450FindSMsb
            Just (_, U32) -> glslStd450FindUMsb
            _ -> glslStd450FindSMsb
      emitExtInst (val.valType) inst [val.valId] st1 fs1
    _ -> Left (CompileError "firstLeadingBit expects one argument" Nothing Nothing)

emitSaturateBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSaturateBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (val.valType)
      (st2, zeroVal) <- emitConstFloatSplat (val.valType) 0.0 st1
      (st3, oneVal) <- emitConstFloatSplat (val.valType) 1.0 st2
      emitExtInst (val.valType) glslStd450FClamp [val.valId, zeroVal.valId, oneVal.valId] st3 fs1
    _ -> Left (CompileError "saturate expects one argument" Nothing Nothing)

emitIntNegate :: Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitIntNegate val st fs =
  case val.valType of
    TLScalar I32 _ _ -> do
      let (tyId, st1) = emitTypeFromLayout st (val.valType)
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opSNegate [tyId, resId, val.valId]) fs
      Right (st2, fs1, Value (val.valType) resId)
    TLVector _ I32 _ _ -> do
      let (tyId, st1) = emitTypeFromLayout st (val.valType)
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opSNegate [tyId, resId, val.valId]) fs
      Right (st2, fs1, Value (val.valType) resId)
    _ -> Left (CompileError "expected i32 scalar or vector" Nothing Nothing)

emitAbsBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAbsBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case classifyNumeric (val.valType) of
        Just (_, F32) -> emitExtInst (val.valType) glslStd450FAbs [val.valId] st1 fs1
        Just (_, F16) -> emitExtInst (val.valType) glslStd450FAbs [val.valId] st1 fs1
        Just (_, I32) -> do
          (st2, zeroVal) <- emitConstIntSplat (val.valType) 0 st1
          (st3, fs3, condVal) <- emitBinary OpLt (val.valType) (val.valId) (zeroVal.valId) st2 fs1
          (st4, fs4, negVal) <- emitIntNegate val st3 fs3
          emitSelectValue (val.valType) negVal val condVal st4 fs4
        Just (_, U32) -> Right (st1, fs1, val)
        _ -> Left (CompileError "abs expects numeric scalar or vector types" Nothing Nothing)
    _ -> Left (CompileError "abs expects one argument" Nothing Nothing)

emitMinMaxInt :: BinOp -> TypeLayout -> Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMinMaxInt cmpOp layout v1 v2 st fs = do
  (st1, fs1, condVal) <- emitBinary cmpOp layout (v1.valId) (v2.valId) st fs
  emitSelectValue layout v1 v2 condVal st1 fs1

emitMinMaxBuiltin :: Bool -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMinMaxBuiltin isMin args st fs =
  case args of
    [aExpr, bExpr] -> do
      (st1, fs1, v1) <- emitExpr st fs aExpr
      (st2, fs2, v2) <- emitExpr st1 fs1 bExpr
      (st3, fs3, v1', v2', layout) <-
        if v1.valType == v2.valType
          then Right (st2, fs2, v1, v2, v1.valType)
          else coerceBinaryOperands v1 v2 st2 fs2
      case classifyNumeric layout of
        Just (_, F32) ->
          let inst = if isMin then glslStd450FMin else glslStd450FMax
          in emitExtInst layout inst [v1'.valId, v2'.valId] st3 fs3
        Just (_, F16) ->
          let inst = if isMin then glslStd450FMin else glslStd450FMax
          in emitExtInst layout inst [v1'.valId, v2'.valId] st3 fs3
        Just (_, I32) -> do
          let cmpOp = if isMin then OpLt else OpGt
          emitMinMaxInt cmpOp layout v1' v2' st3 fs3
        Just (_, U32) -> do
          let cmpOp = if isMin then OpLt else OpGt
          emitMinMaxInt cmpOp layout v1' v2' st3 fs3
        _ -> Left (CompileError "min/max expect numeric scalar or vector types" Nothing Nothing)
    _ -> Left (CompileError "min/max expect two arguments" Nothing Nothing)

emitClampBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitClampBuiltin args st fs =
  case args of
    [xExpr, lowExpr, highExpr] -> do
      (st1, fs1, vx) <- emitExpr st fs xExpr
      (st2, fs2, vlo) <- emitExpr st1 fs1 lowExpr
      (st3, fs3, vhi) <- emitExpr st2 fs2 highExpr
      let baseLayout = pickBaseLayout st3 [vx, vlo, vhi]
      case classifyNumeric baseLayout of
        Just (_, F32) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          emitExtInst baseLayout glslStd450FClamp [vx'.valId, vlo'.valId, vhi'.valId] st6 fs6
        Just (_, F16) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          emitExtInst baseLayout glslStd450FClamp [vx'.valId, vlo'.valId, vhi'.valId] st6 fs6
        Just (_, I32) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          (st7, fs7, maxVal) <- emitMinMaxInt OpGt baseLayout vx' vlo' st6 fs6
          emitMinMaxInt OpLt baseLayout maxVal vhi' st7 fs7
        Just (_, U32) -> do
          (st4, fs4, vx') <- coerceValueToLayout baseLayout vx st3 fs3
          (st5, fs5, vlo') <- coerceValueToLayout baseLayout vlo st4 fs4
          (st6, fs6, vhi') <- coerceValueToLayout baseLayout vhi st5 fs5
          (st7, fs7, maxVal) <- emitMinMaxInt OpGt baseLayout vx' vlo' st6 fs6
          emitMinMaxInt OpLt baseLayout maxVal vhi' st7 fs7
        _ -> Left (CompileError "clamp expects numeric scalar or vector types" Nothing Nothing)
    _ -> Left (CompileError "clamp expects three arguments" Nothing Nothing)

emitSignBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSignBuiltin args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case classifyNumeric (val.valType) of
        Just (_, F32) -> emitSignFloat (val.valType) val st1 fs1
        Just (_, F16) -> emitSignFloat (val.valType) val st1 fs1
        Just (_, I32) -> emitSignInt (val.valType) val st1 fs1
        _ -> Left (CompileError "sign expects i32 or float scalar/vector types" Nothing Nothing)
    _ -> Left (CompileError "sign expects one argument" Nothing Nothing)
  where
    emitSignFloat layout val st1 fs1 = do
      (st2, zeroVal) <- emitConstFloatSplat layout 0.0 st1
      (st3, oneVal) <- emitConstFloatSplat layout 1.0 st2
      (st4, negOneVal) <- emitConstFloatSplat layout (-1.0) st3
      (st5, fs2, condPos) <- emitBinary OpGt layout (val.valId) (zeroVal.valId) st4 fs1
      (st6, fs3, condNeg) <- emitBinary OpLt layout (val.valId) (zeroVal.valId) st5 fs2
      (st7, fs4, posVal) <- emitSelectValue layout oneVal zeroVal condPos st6 fs3
      emitSelectValue layout negOneVal posVal condNeg st7 fs4
    emitSignInt layout val st1 fs1 = do
      (st2, zeroVal) <- emitConstIntSplat layout 0 st1
      (st3, oneVal) <- emitConstIntSplat layout 1 st2
      (st4, negOneVal) <- emitConstIntSplat layout (-1) st3
      (st5, fs2, condPos) <- emitBinary OpGt layout (val.valId) (zeroVal.valId) st4 fs1
      (st6, fs3, condNeg) <- emitBinary OpLt layout (val.valId) (zeroVal.valId) st5 fs2
      (st7, fs4, posVal) <- emitSelectValue layout oneVal zeroVal condPos st6 fs3
      emitSelectValue layout negOneVal posVal condNeg st7 fs4

emitBitUnary :: Word16 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBitUnary opcode args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      ensureIntNumeric (val.valType)
      let (tyId, st2) = emitTypeFromLayout st1 (val.valType)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opcode [tyId, resId, val.valId]) fs1
      Right (st3, fs2, Value (val.valType) resId)
    _ -> Left (CompileError "bitwise builtin expects one argument" Nothing Nothing)

emitCountLeadingZeros :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCountLeadingZeros args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (st2, fs2, valU, layoutU, origLayout) <- toUnsignedValue val st1 fs1
      let bitWidth = intBitWidth layoutU
      (st3, zeroVal) <- emitConstIntSplat layoutU 0 st2
      (st4, fs3, condZero) <- emitBinary OpEq layoutU (valU.valId) (zeroVal.valId) st3 fs2
      (st5, bitWidthVal) <- emitConstIntSplat layoutU bitWidth st4
      (st6, oneLessVal) <- emitConstIntSplat layoutU (bitWidth - 1) st5
      (st7, fs4, msbVal) <- emitExtInst layoutU glslStd450FindUMsb [valU.valId] st6 fs3
      (st8, fs5, clzVal) <- emitBinary OpSub layoutU (oneLessVal.valId) (msbVal.valId) st7 fs4
      (st9, fs6, resU) <- emitSelectValue layoutU bitWidthVal clzVal condZero st8 fs5
      fromUnsignedValue origLayout resU st9 fs6
    _ -> Left (CompileError "countLeadingZeros expects one argument" Nothing Nothing)

emitCountTrailingZeros :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCountTrailingZeros args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      (st2, fs2, valU, layoutU, origLayout) <- toUnsignedValue val st1 fs1
      let bitWidth = intBitWidth layoutU
      (st3, zeroVal) <- emitConstIntSplat layoutU 0 st2
      (st4, fs3, condZero) <- emitBinary OpEq layoutU (valU.valId) (zeroVal.valId) st3 fs2
      (st5, bitWidthVal) <- emitConstIntSplat layoutU bitWidth st4
      (st6, fs4, lsbVal) <- emitExtInst layoutU glslStd450FindILsb [valU.valId] st5 fs3
      (st7, fs5, resU) <- emitSelectValue layoutU bitWidthVal lsbVal condZero st6 fs4
      fromUnsignedValue origLayout resU st7 fs5
    _ -> Left (CompileError "countTrailingZeros expects one argument" Nothing Nothing)

emitDot4Packed :: Bool -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDot4Packed signed args st fs =
  case args of
    [aExpr, bExpr] -> do
      (st1, fs1, aVal) <- emitExpr st fs aExpr
      (st2, fs2, bVal) <- emitExpr st1 fs1 bExpr
      let targetScalar = if signed then I32 else U32
          (a, sz) = scalarLayout targetScalar
          layout = TLScalar targetScalar a sz
      (st3, fs3, aPacked) <- coercePackedScalar "dot4 packed value" layout aVal st2 fs2
      (st4, fs4, bPacked) <- coercePackedScalar "dot4 packed value" layout bVal st3 fs3
      (st5, sum0) <- emitConstIntSplat layout 0 st4
      let offsets = [0, 8, 16, 24] :: [Integer]
          opcode = if signed then opBitFieldSExtract else opBitFieldUExtract
          go st' fs' acc [] = Right (st', fs', acc)
          go st' fs' acc (off:rest) = do
            (stA, fsA, aLane) <- emitBitFieldExtractConst opcode layout aPacked off 8 st' fs'
            (stB, fsB, bLane) <- emitBitFieldExtractConst opcode layout bPacked off 8 stA fsA
            (stC, fsC, prod) <- emitBinary OpMul layout (aLane.valId) (bLane.valId) stB fsB
            (stD, fsD, sum') <- emitBinary OpAdd layout (acc.valId) (prod.valId) stC fsC
            go stD fsD sum' rest
      (st6, fs6, sumVal) <- go st5 fs4 sum0 offsets
      Right (st6, fs6, sumVal)
    _ -> Left (CompileError "dot4 packed expects two arguments" Nothing Nothing)
  where
    coercePackedScalar label layout val st1 fs1 =
      case val.valType of
        TLScalar I32 _ _ ->
          if layout == val.valType
            then Right (st1, fs1, val)
            else emitBitcastValue layout val st1 fs1
        TLScalar U32 _ _ ->
          if layout == val.valType
            then Right (st1, fs1, val)
            else emitBitcastValue layout val st1 fs1
        _ -> Left (CompileError (label <> " must be i32 or u32 scalar") Nothing Nothing)

emitBitFieldExtractConst :: Word16 -> TypeLayout -> Value -> Integer -> Integer -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBitFieldExtractConst opcode layout baseVal offset count st fs = do
  (st1, offsetVal) <- emitConstIntSplat layout offset st
  (st2, countVal) <- emitConstIntSplat layout count st1
  let (tyId, st3) = emitTypeFromLayout st2 layout
  let (resId, st4) = freshId st3
  let fs1 = addFuncInstr (Instr opcode [tyId, resId, baseVal.valId, offsetVal.valId, countVal.valId]) fs
  Right (st4, fs1, Value layout resId)

toUnsignedValue :: Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value, TypeLayout, TypeLayout)
toUnsignedValue val st fs =
  case val.valType of
    TLScalar U32 _ _ -> Right (st, fs, val, val.valType, val.valType)
    TLScalar I32 _ _ -> do
      let (a, sz) = scalarLayout U32
          layoutU = TLScalar U32 a sz
      (st1, fs1, valU) <- emitBitcastValue layoutU val st fs
      Right (st1, fs1, valU, layoutU, val.valType)
    TLVector _ U32 _ _ -> Right (st, fs, val, val.valType, val.valType)
    TLVector n I32 _ _ -> do
      let (a, sz) = vectorLayout U32 n
          layoutU = TLVector n U32 a sz
      (st1, fs1, valU) <- emitBitcastValue layoutU val st fs
      Right (st1, fs1, valU, layoutU, val.valType)
    _ -> Left (CompileError "countLeadingZeros/countTrailingZeros expect i32 or u32 scalar or vector types" Nothing Nothing)

fromUnsignedValue :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
fromUnsignedValue origLayout valU st fs =
  case origLayout of
    TLScalar I32 _ _ -> emitBitcastValue origLayout valU st fs
    TLVector _ I32 _ _ -> emitBitcastValue origLayout valU st fs
    _ -> Right (st, fs, valU)

intBitWidth :: TypeLayout -> Integer
intBitWidth layout =
  case layout of
    TLScalar U32 _ _ -> 32
    TLVector _ U32 _ _ -> 32
    _ -> 32

resolveBitcastLayout :: Type -> Either CompileError TypeLayout
resolveBitcastLayout ty =
  case ty of
    TyScalar scalar ->
      case scalar of
        Bool -> Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        _ ->
          let (a, sz) = scalarLayout scalar
          in Right (TLScalar scalar a sz)
    TyVector n scalar ->
      case scalar of
        Bool -> Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        _ ->
          let (a, sz) = vectorLayout scalar n
          in Right (TLVector n scalar a sz)
    _ -> Left (CompileError "bitcast expects a scalar or vector type" Nothing Nothing)

bitcastLayoutInfo :: TypeLayout -> Either CompileError (Int, Word32, Scalar)
bitcastLayoutInfo layout =
  case layout of
    TLScalar scalar _ _ ->
      if scalar == Bool
        then Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        else
          let (_, sz) = scalarLayout scalar
          in Right (1, sz, scalar)
    TLVector n scalar _ _ ->
      if scalar == Bool
        then Left (CompileError "bitcast does not support bool types" Nothing Nothing)
        else
          let (_, sz) = scalarLayout scalar
          in Right (n, sz, scalar)
    _ -> Left (CompileError "bitcast expects a scalar or vector type" Nothing Nothing)

normalizeBitFieldIndex :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
normalizeBitFieldIndex baseLayout idxVal st fs =
  case baseLayout of
    TLScalar baseScalar _ _ -> do
      ensureIndexType (idxVal.valType)
      case idxVal.valType of
        TLScalar s _ _ | s == baseScalar -> Right (st, fs, idxVal)
        TLScalar s _ _ -> emitScalarConvert s baseScalar idxVal st fs
        _ -> Left (CompileError "bitfield indices must be scalar" Nothing Nothing)
    TLVector n baseScalar _ _ ->
      case idxVal.valType of
        TLScalar s _ _ -> do
          ensureIndexType (idxVal.valType)
          (st1, fs1, scalarVal) <-
            if s == baseScalar
              then Right (st, fs, idxVal)
              else emitScalarConvert s baseScalar idxVal st fs
          let (a, sz) = vectorLayout baseScalar n
          let layout = TLVector n baseScalar a sz
          emitSplatVector layout (scalarVal.valId) st1 fs1
        TLVector m s _ _ | m == n && s == baseScalar -> Right (st, fs, idxVal)
        TLVector {} -> Left (CompileError "bitfield indices must match vector size and scalar type" Nothing Nothing)
        _ -> Left (CompileError "bitfield indices must be scalar or matching vector" Nothing Nothing)
    _ -> Left (CompileError "bitfield indices require integer scalars or vectors" Nothing Nothing)

emitBitcastValue :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBitcastValue target val st fs = do
  let (tyId, st1) = emitTypeFromLayout st target
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opBitcast [tyId, resId, val.valId]) fs
  Right (st2, fs1, Value target resId)

coerceExpToI32 :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
coerceExpToI32 baseLayout expVal st fs =
  case baseLayout of
    TLScalar {} ->
      case expVal.valType of
        TLScalar I32 _ _ -> Right (st, fs, expVal)
        TLScalar U32 _ _ -> emitScalarConvert U32 I32 expVal st fs
        _ -> Left (CompileError "ldexp exponent must be i32 or u32 scalar" Nothing Nothing)
    TLVector n _ _ _ ->
      case expVal.valType of
        TLScalar s _ _ -> do
          (st1, fs1, scalarVal) <- case s of
            I32 -> Right (st, fs, expVal)
            U32 -> emitScalarConvert U32 I32 expVal st fs
            _ -> Left (CompileError "ldexp exponent must be i32 or u32" Nothing Nothing)
          let (a, sz) = vectorLayout I32 n
          let layout = TLVector n I32 a sz
          emitSplatVector layout (scalarVal.valId) st1 fs1
        TLVector m s _ _ | m == n ->
          case s of
            I32 -> Right (st, fs, expVal)
            U32 ->
              let (a, sz) = vectorLayout I32 n
                  layout = TLVector n I32 a sz
              in emitBitcastValue layout expVal st fs
            _ -> Left (CompileError "ldexp exponent must be i32 or u32" Nothing Nothing)
        _ -> Left (CompileError "ldexp exponent must be i32 or u32" Nothing Nothing)
    _ -> Left (CompileError "ldexp expects a float scalar or vector" Nothing Nothing)

emitExtractBitsBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitExtractBitsBuiltin args st fs =
  case args of
    [baseExpr, offsetExpr, countExpr] -> do
      (st1, fs1, baseVal) <- emitExpr st fs baseExpr
      ensureIntNumeric (baseVal.valType)
      (st2, fs2, offsetVal) <- emitExpr st1 fs1 offsetExpr
      (st3, fs3, countVal) <- emitExpr st2 fs2 countExpr
      (st4, fs4, offsetVal') <- normalizeBitFieldIndex (baseVal.valType) offsetVal st3 fs3
      (st5, fs5, countVal') <- normalizeBitFieldIndex (baseVal.valType) countVal st4 fs4
      scalar <- case classifyNumeric (baseVal.valType) of
        Just (_, s) -> Right s
        Nothing -> Left (CompileError "extractBits expects integer types" Nothing Nothing)
      opcode <- case scalar of
        I32 -> Right opBitFieldSExtract
        U32 -> Right opBitFieldUExtract
        _ -> Left (CompileError "extractBits expects i32 or u32 types" Nothing Nothing)
      let (tyId, st6) = emitTypeFromLayout st5 (baseVal.valType)
      let (resId, st7) = freshId st6
      let fs6 = addFuncInstr (Instr opcode [tyId, resId, baseVal.valId, offsetVal'.valId, countVal'.valId]) fs5
      Right (st7, fs6, Value (baseVal.valType) resId)
    _ -> Left (CompileError "extractBits expects (e, offset, count)" Nothing Nothing)

emitInsertBitsBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitInsertBitsBuiltin args st fs =
  case args of
    [baseExpr, insertExpr, offsetExpr, countExpr] -> do
      (st1, fs1, baseVal) <- emitExpr st fs baseExpr
      (st2, fs2, insertVal) <- emitExpr st1 fs1 insertExpr
      (st3, fs3, offsetVal) <- emitExpr st2 fs2 offsetExpr
      (st4, fs4, countVal) <- emitExpr st3 fs3 countExpr
      (st5, fs5, baseVal', insertVal', layout) <-
        if baseVal.valType == insertVal.valType
          then Right (st4, fs4, baseVal, insertVal, baseVal.valType)
          else coerceBinaryOperands baseVal insertVal st4 fs4
      ensureIntNumeric layout
      (st6, fs6, offsetVal') <- normalizeBitFieldIndex layout offsetVal st5 fs5
      (st7, fs7, countVal') <- normalizeBitFieldIndex layout countVal st6 fs6
      let (tyId, st8) = emitTypeFromLayout st7 layout
      let (resId, st9) = freshId st8
      let fs8 = addFuncInstr (Instr opBitFieldInsert [tyId, resId, baseVal'.valId, insertVal'.valId, offsetVal'.valId, countVal'.valId]) fs7
      Right (st9, fs8, Value layout resId)
    _ -> Left (CompileError "insertBits expects (base, insert, offset, count)" Nothing Nothing)

emitArrayLengthBuiltin :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitArrayLengthBuiltin args st fs =
  case args of
    [expr] ->
      case exprToLValue expr of
        Just lv -> emitArrayLengthLValue lv st fs
        Nothing -> Left (CompileError "arrayLength expects an addressable runtime array" Nothing Nothing)
    _ -> Left (CompileError "arrayLength expects one argument" Nothing Nothing)

emitArrayLengthLValue :: LValue -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitArrayLengthLValue lv st fs =
  case lv of
    LVField _ base field -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      when (baseInfo.viStorage /= storageClassStorageBuffer) $
        Left (CompileError "arrayLength requires a storage buffer struct" Nothing Nothing)
      case baseInfo.viType of
        TLStruct _ fields _ _ -> do
          (ix, fieldLayout) <- findField (textToString field) fields
          when (ix /= length fields - 1) $
            Left (CompileError "runtime arrays must be the last struct member" Nothing Nothing)
          case fieldLayout of
            TLArray Nothing _ _ _ _ -> do
              let (a, sz) = scalarLayout U32
              let layout = TLScalar U32 a sz
              let (tyId, st2) = emitTypeFromLayout st1 layout
              let (resId, st3) = freshId st2
              let fs2 = addFuncInstr (Instr opArrayLength [tyId, resId, baseInfo.viPtrId, fromIntegral ix]) fs1
              Right (st3, fs2, Value layout resId)
            _ -> Left (CompileError "arrayLength expects a runtime array field" Nothing Nothing)
        _ -> Left (CompileError "arrayLength expects a struct field" Nothing Nothing)
    _ -> Left (CompileError "arrayLength expects a struct runtime array field" Nothing Nothing)

emitFunctionCallByName :: Text -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Maybe Value)
emitFunctionCallByName name args st fs = do
  (st1, fs1, vals) <- emitExprList st fs args
  let argCount = length vals
  let valTypes = map (.valType) vals
  let named = Map.findWithDefault [] name st.gsFunctionsByName
  let candidates = filter (\fi -> length (fi.fiParams) == argCount) named
  let exactMatches = filter (\fi -> and (zipWith (==) (fi.fiParams) valTypes)) candidates
  let coercibleMatches =
        filter
          (\fi -> and (zipWith (argCoercible st1) vals (fi.fiParams)))
          candidates
  case uniqueMatch exactMatches of
    Left err -> Left err
    Right (Just fi) -> emitFunctionCall fi vals st1 fs1
    Right Nothing ->
      case uniqueMatch coercibleMatches of
        Left err -> Left err
        Right (Just fi) -> do
          (st2, fs2, vals') <- coerceArgsToLayouts vals (fi.fiParams) st1 fs1
          emitFunctionCall fi vals' st2 fs2
        Right Nothing -> Left (CompileError ("unsupported call: " <> textToString name) Nothing Nothing)
  where
    uniqueMatch matches =
      case matches of
        [] -> Right Nothing
        [fi] -> Right (Just fi)
        _ -> Left (CompileError ("ambiguous overload for " <> textToString name) Nothing Nothing)

argCoercible :: GenState -> Value -> TypeLayout -> Bool
argCoercible st actual expected =
  either
    (const fallback)
    (const True)
    (ensureTypeMatch expected (actual.valType))
  where
    fallback =
      case (actual.valType, expected) of
        (TLScalar {}, TLScalar {}) -> isConstLiteral st actual
        _ -> False

emitFunctionCall :: FunctionInfo -> [Value] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Maybe Value)
emitFunctionCall fnInfo vals st fs = do
  let expected = length (fnInfo.fiParams)
      got = length vals
  when (got /= expected) $
    Left (CompileError "function arity mismatch" Nothing Nothing)
  zipWithM_ (\val ty -> ensureTypeMatch (val.valType) ty) vals (fnInfo.fiParams)
  let (retTyId, st1) = case fnInfo.fiReturn of
        Nothing -> emitVoidType st
        Just layout -> emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let argIds = map (.valId) vals
  let instr = Instr opFunctionCall (retTyId : resId : fnInfo.fiId : argIds)
  let fs1 = addFuncInstr instr fs
  case fnInfo.fiReturn of
    Nothing -> Right (st2, fs1, Nothing)
    Just layout -> Right (st2, fs1, Just (Value layout resId))

emitZeroConstScalar :: Scalar -> GenState -> Either CompileError (GenState, Value)
emitZeroConstScalar scalar st =
  case scalar of
    F16 -> do
      let (cid, st1) = emitConstF16 st 0
      let (a, sz) = scalarLayout F16
      Right (st1, Value (TLScalar F16 a sz) cid)
    F32 -> do
      let (cid, st1) = emitConstF32 st 0
      let (a, sz) = scalarLayout F32
      Right (st1, Value (TLScalar F32 a sz) cid)
    I32 -> do
      (cid, st1) <- emitConstIntScalar I32 0 st
      let (a, sz) = scalarLayout I32
      Right (st1, Value (TLScalar I32 a sz) cid)
    U32 -> do
      (cid, st1) <- emitConstIntScalar U32 0 st
      let (a, sz) = scalarLayout U32
      Right (st1, Value (TLScalar U32 a sz) cid)
    Bool -> do
      let (cid, st1) = emitConstBool st False
      let (a, sz) = scalarLayout Bool
      Right (st1, Value (TLScalar Bool a sz) cid)

emitZeroConstValue :: TypeLayout -> GenState -> Either CompileError (GenState, Value)
emitZeroConstValue layout st =
  case layout of
    TLScalar scalar _ _ -> emitZeroConstScalar scalar st
    TLVector n scalar _ _ -> do
      (st1, zeroScalar) <- emitZeroConstScalar scalar st
      let (cid, st2) = emitConstComposite layout (replicate n zeroScalar.valId) st1
      Right (st2, Value layout cid)
    TLMatrix cols rows scalar _ _ _ -> do
      let (va, vsz) = vectorLayout scalar rows
      let vecLayout = TLVector rows scalar va vsz
      (st1, zeroCol) <- emitZeroConstValue vecLayout st
      let (cid, st2) = emitConstComposite layout (replicate cols zeroCol.valId) st1
      Right (st2, Value layout cid)
    TLArray (Just n) _ elemLayout _ _ -> do
      (st1, zeroElem) <- emitZeroConstValue elemLayout st
      let (cid, st2) = emitConstComposite layout (replicate n zeroElem.valId) st1
      Right (st2, Value layout cid)
    TLArray Nothing _ _ _ _ ->
      Left (CompileError "runtime arrays cannot be zero-constructed" Nothing Nothing)
    TLStruct _ fields _ _ -> do
      (st1, values) <- foldM step (st, []) fields
      let (cid, st2) = emitConstComposite layout (map (.valId) values) st1
      Right (st2, Value layout cid)
      where
        step (st', acc) field = do
          (st1, v) <- emitZeroConstValue field.flType st'
          Right (st1, acc <> [v])
    TLAtomic _ ->
      Left (CompileError "atomic types cannot be zero-constructed" Nothing Nothing)
    TLPointer _ _ _ ->
      Left (CompileError "pointer types cannot be zero-constructed" Nothing Nothing)
    TLSampler ->
      Left (CompileError "sampler types cannot be zero-constructed" Nothing Nothing)
    TLSamplerComparison ->
      Left (CompileError "sampler types cannot be zero-constructed" Nothing Nothing)
    TLTexture1D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture1DArray _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture2D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture2DArray _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture3D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureCube _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureCubeArray _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureMultisampled2D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepth2D ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepth2DArray ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepthCube ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepthCubeArray ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepthMultisampled2D ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture1D _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture2D _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture2DArray _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture3D _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)

emitZeroSpecConstScalar :: Scalar -> GenState -> Either CompileError (GenState, Value)
emitZeroSpecConstScalar scalar st =
  case scalar of
    F16 -> do
      (cid, st1) <- emitSpecConstFloatScalar F16 0 st
      let (a, sz) = scalarLayout F16
      Right (st1, Value (TLScalar F16 a sz) cid)
    F32 -> do
      (cid, st1) <- emitSpecConstFloatScalar F32 0 st
      let (a, sz) = scalarLayout F32
      Right (st1, Value (TLScalar F32 a sz) cid)
    I32 -> do
      (cid, st1) <- emitSpecConstIntScalar I32 0 st
      let (a, sz) = scalarLayout I32
      Right (st1, Value (TLScalar I32 a sz) cid)
    U32 -> do
      (cid, st1) <- emitSpecConstIntScalar U32 0 st
      let (a, sz) = scalarLayout U32
      Right (st1, Value (TLScalar U32 a sz) cid)
    Bool -> do
      let (cid, st1) = emitSpecConstBool st False
      let (a, sz) = scalarLayout Bool
      Right (st1, Value (TLScalar Bool a sz) cid)

emitZeroSpecConstValue :: TypeLayout -> GenState -> Either CompileError (GenState, Value)
emitZeroSpecConstValue layout st =
  case layout of
    TLScalar scalar _ _ -> emitZeroSpecConstScalar scalar st
    TLVector n scalar _ _ -> do
      (st1, zeroScalar) <- emitZeroSpecConstScalar scalar st
      let (cid, st2) = emitSpecConstComposite layout (replicate n zeroScalar.valId) st1
      Right (st2, Value layout cid)
    TLMatrix cols rows scalar _ _ _ -> do
      let (va, vsz) = vectorLayout scalar rows
      let vecLayout = TLVector rows scalar va vsz
      (st1, zeroCol) <- emitZeroSpecConstValue vecLayout st
      let (cid, st2) = emitSpecConstComposite layout (replicate cols zeroCol.valId) st1
      Right (st2, Value layout cid)
    TLArray (Just n) _ elemLayout _ _ -> do
      (st1, zeroElem) <- emitZeroSpecConstValue elemLayout st
      let (cid, st2) = emitSpecConstComposite layout (replicate n zeroElem.valId) st1
      Right (st2, Value layout cid)
    TLArray Nothing _ _ _ _ ->
      Left (CompileError "runtime arrays cannot be zero-constructed" Nothing Nothing)
    TLStruct _ fields _ _ -> do
      (st1, values) <- foldM step (st, []) fields
      let (cid, st2) = emitSpecConstComposite layout (map (.valId) values) st1
      Right (st2, Value layout cid)
      where
        step (st', acc) field = do
          (st1, v) <- emitZeroSpecConstValue field.flType st'
          Right (st1, acc <> [v])
    TLAtomic _ ->
      Left (CompileError "atomic types cannot be zero-constructed" Nothing Nothing)
    TLPointer _ _ _ ->
      Left (CompileError "pointer types cannot be zero-constructed" Nothing Nothing)
    TLSampler ->
      Left (CompileError "sampler types cannot be zero-constructed" Nothing Nothing)
    TLSamplerComparison ->
      Left (CompileError "sampler types cannot be zero-constructed" Nothing Nothing)
    TLTexture1D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture1DArray _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture2D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture2DArray _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTexture3D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureCube _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureCubeArray _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureMultisampled2D _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepth2D ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepth2DArray ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepthCube ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepthCubeArray ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLTextureDepthMultisampled2D ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture1D _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture2D _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture2DArray _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)
    TLStorageTexture3D _ _ ->
      Left (CompileError "texture types cannot be zero-constructed" Nothing Nothing)

resolveFixedArrayCtorLength :: ArrayLen -> Either CompileError Int
resolveFixedArrayCtorLength arrLen =
  case arrLen of
    ArrayLenFixed n ->
      if n > 0
        then Right n
        else Left (CompileError "array length must be positive" Nothing Nothing)
    ArrayLenExpr _ ->
      Left (CompileError "array constructor size must be resolved before codegen" Nothing Nothing)
    ArrayLenRuntime ->
      Left (CompileError "array constructor requires a fixed size" Nothing Nothing)

emitTypedArrayCtor :: Type -> ArrayLen -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTypedArrayCtor elemTy arrLen args st fs = do
  elemLayout <- resolveTypeLayoutInState st elemTy
  len <- resolveFixedArrayCtorLength arrLen
  when (containsResource elemLayout) $
    Left (CompileError "arrays of resources are not supported" Nothing Nothing)
  when (containsAtomic elemLayout) $
    Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
  let elemAlign = layoutAlign elemLayout
  let elemSize = layoutSize elemLayout
  let stride = roundUp elemSize elemAlign
  let total = stride * fromIntegral len
  let layout = TLArray (Just len) stride elemLayout elemAlign total
  case args of
    [] -> do
      (st1, zeroVal) <- emitZeroConstValue layout st
      Right (st1, fs, zeroVal)
    _ -> do
      (st1, fs1, vals) <- emitExprList st fs args
      when (length vals /= len) $
        Left (CompileError "array constructor arity mismatch" Nothing Nothing)
      (st2, fs2, vals') <- coerceValuesToLayout elemLayout vals st1 fs1
      let (tyId, st3) = emitTypeFromLayout st2 layout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) vals')) fs2
      Right (st4, fs3, Value layout resId)

emitConstTypedArrayCtor :: Type -> ArrayLen -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitConstTypedArrayCtor elemTy arrLen args st = do
  elemLayout <- resolveTypeLayoutInState st elemTy
  len <- resolveFixedArrayCtorLength arrLen
  when (containsResource elemLayout) $
    Left (CompileError "arrays of resources are not supported" Nothing Nothing)
  when (containsAtomic elemLayout) $
    Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
  let elemAlign = layoutAlign elemLayout
  let elemSize = layoutSize elemLayout
  let stride = roundUp elemSize elemAlign
  let total = stride * fromIntegral len
  let layout = TLArray (Just len) stride elemLayout elemAlign total
  case args of
    [] ->
      emitZeroConstValue layout st
    _ -> do
      (st1, vals) <- emitConstExprList st args
      when (length vals /= len) $
        Left (CompileError "array constructor arity mismatch" Nothing Nothing)
      (st2, vals') <- coerceConstValuesToLayout elemLayout vals st1
      let (cid, st3) = emitConstComposite layout (map (.valId) vals') st2
      Right (st3, Value layout cid)

emitSpecConstTypedArrayCtor :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Type -> ArrayLen -> [Expr] -> GenState -> Either CompileError (GenState, Value)
emitSpecConstTypedArrayCtor ctx constIndex fnIndex structIndex elemTy arrLen args st = do
  elemLayout <- resolveTypeLayoutInState st elemTy
  len <- resolveFixedArrayCtorLength arrLen
  when (containsResource elemLayout) $
    Left (CompileError "arrays of resources are not supported" Nothing Nothing)
  when (containsAtomic elemLayout) $
    Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
  let elemAlign = layoutAlign elemLayout
  let elemSize = layoutSize elemLayout
  let stride = roundUp elemSize elemAlign
  let total = stride * fromIntegral len
  let layout = TLArray (Just len) stride elemLayout elemAlign total
  case args of
    [] ->
      emitZeroSpecConstValue layout st
    _ -> do
      (st1, vals) <- emitSpecConstExprList ctx constIndex fnIndex structIndex st args
      when (length vals /= len) $
        Left (CompileError "array constructor arity mismatch" Nothing Nothing)
      (st2, vals') <- coerceSpecConstValuesToLayout elemLayout vals st1
      let (cid, st3) = emitSpecConstComposite layout (map (.valId) vals') st2
      Right (st3, Value layout cid)

emitVectorCtor :: Int -> Maybe Scalar -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVectorCtor n targetScalar args st fs =
  if null args
    then
      case targetScalar of
        Nothing -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        Just scalar -> do
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          (st1, v) <- emitZeroConstValue layout st
          Right (st1, fs, v)
    else do
      (st1, fs1, vals) <- emitExprList st fs args
      let singleScalar =
            case vals of
              [v] ->
                case v.valType of
                  TLScalar {} -> True
                  _ -> False
              _ -> False
      (st2, fs2, flat) <- flattenValuesToScalars vals st1 fs1
      case flat of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        _ -> do
          (st3, fs3, vals', scalar) <-
            case targetScalar of
              Just s -> do
                (st', fs', coerced) <- coerceValuesToScalar s flat st2 fs2
                Right (st', fs', coerced, s)
              Nothing -> do
                let baseLayout = pickBaseLayout st2 flat
                (st', fs', coerced) <- coerceValuesToLayout baseLayout flat st2 fs2
                scalar <- case baseLayout of
                  TLScalar s _ _ -> Right s
                  _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
                Right (st', fs', coerced, scalar)
          let filled =
                case (singleScalar, vals') of
                  (True, [v]) -> replicate n v
                  _ -> vals'
          when (length filled /= n) $
            Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
          let (align, size) = vectorLayout scalar n
          let layout = TLVector n scalar align size
          let (tyId, st4) = emitTypeFromLayout st3 layout
          let (resId, st5) = freshId st4
          let fs4 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) filled)) fs3
          Right (st5, fs4, Value layout resId)

emitMatrixCtor :: Int -> Int -> Maybe Scalar -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitMatrixCtor cols rows targetScalar args st fs =
  if null args
    then
      case targetScalar of
        Nothing -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        Just scalar -> do
          let layout = matrixLayout cols rows scalar
          (st1, v) <- emitZeroConstValue layout st
          Right (st1, fs, v)
    else do
      (st1, fs1, vals) <- emitExprList st fs args
      case vals of
        [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        [v] | isScalarLayout v.valType -> do
          (st2, fs2, scalarVal, scalar) <- coerceSingleScalar v st1 fs1
          (st3, zeroVal) <- emitConstZeroScalar scalar st2
          let (a, sz) = vectorLayout scalar rows
          let vecLayout = TLVector rows scalar a sz
          (st4, fs4, colsVals) <- buildDiagColumns vecLayout scalarVal zeroVal st3 fs2
          let layout = matrixLayout cols rows scalar
          let (tyId, st5) = emitTypeFromLayout st4 layout
          let (resId, st6) = freshId st5
          let fs5 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) colsVals)) fs4
          Right (st6, fs5, Value layout resId)
        _ -> do
          let scalarCount = cols * rows
          (st2, fs2, flat) <- flattenMatrixValues rows vals st1 fs1
          (st3, fs3, vals', scalar) <-
            case targetScalar of
              Just s -> do
                (st', fs', coerced) <- coerceValuesToScalar s flat st2 fs2
                Right (st', fs', coerced, s)
              Nothing -> do
                let baseLayout = pickBaseLayout st2 flat
                (st', fs', coerced) <- coerceValuesToLayout baseLayout flat st2 fs2
                scalar <- case baseLayout of
                  TLScalar s _ _ -> Right s
                  _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
                Right (st', fs', coerced, scalar)
          when (length vals' /= scalarCount) $
            Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
          let (a, sz) = vectorLayout scalar rows
          let vecLayout = TLVector rows scalar a sz
          (st4, fs4, colsVals) <- buildColumns vecLayout vals' st3 fs3
          let layout = matrixLayout cols rows scalar
          let (tyId, st5) = emitTypeFromLayout st4 layout
          let (resId, st6) = freshId st5
          let fs5 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) colsVals)) fs4
          Right (st6, fs5, Value layout resId)
  where
    isScalarLayout layout =
      case layout of
        TLScalar {} -> True
        _ -> False
    coerceSingleScalar v st' fs' =
      case targetScalar of
        Just s -> do
          (st1, fs1, coerced) <- coerceValuesToScalar s [v] st' fs'
          case coerced of
            [v'] -> Right (st1, fs1, v', s)
            _ -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
        Nothing ->
          case v.valType of
            TLScalar s _ _ -> Right (st', fs', v, s)
            _ -> Left (CompileError "matrix constructor expects a scalar" Nothing Nothing)
    emitConstZeroScalar scalar st' =
      case scalar of
        F16 -> do
          let (cid, st1) = emitConstF16 st' 0
          let (a, sz) = scalarLayout F16
          Right (st1, Value (TLScalar F16 a sz) cid)
        F32 -> do
          let (cid, st1) = emitConstF32 st' 0
          let (a, sz) = scalarLayout F32
          Right (st1, Value (TLScalar F32 a sz) cid)
        I32 -> do
          (cid, st1) <- emitConstIntScalar I32 0 st'
          let (a, sz) = scalarLayout I32
          Right (st1, Value (TLScalar I32 a sz) cid)
        U32 -> do
          (cid, st1) <- emitConstIntScalar U32 0 st'
          let (a, sz) = scalarLayout U32
          Right (st1, Value (TLScalar U32 a sz) cid)
        Bool -> do
          let (cid, st1) = emitConstBool st' False
          let (a, sz) = scalarLayout Bool
          Right (st1, Value (TLScalar Bool a sz) cid)
    flattenMatrixValues rows' vals st' fs' = go st' fs' [] vals
      where
        go st1 fs1 acc [] = Right (st1, fs1, reverse acc)
        go st1 fs1 acc (v:vs) =
          case v.valType of
            TLScalar {} -> go st1 fs1 (v:acc) vs
            TLVector n _ _ _
              | n == rows' -> do
                  (st2, fs2, comps) <- extractComponents 0 n v st1 fs1
                  go st2 fs2 (reverse comps <> acc) vs
              | otherwise ->
                  Left (CompileError "matrix constructor expects column vectors matching the row count" Nothing Nothing)
            _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
        extractComponents idx count vecVal st1 fs1
          | idx >= count = Right (st1, fs1, [])
          | otherwise = do
              (st2, fs2, comp) <- emitVectorComponent vecVal (fromIntegral idx) st1 fs1
              (st3, fs3, rest) <- extractComponents (idx + 1) count vecVal st2 fs2
              Right (st3, fs3, comp : rest)
    buildDiagColumns vecLayout diagVal zeroVal st' fs' =
      go 0 st' fs'
      where
        go colIx st1 fs1
          | colIx >= cols = Right (st1, fs1, [])
          | otherwise = do
              let elems =
                    [ if rowIx == colIx then diagVal else zeroVal
                    | rowIx <- [0 .. rows - 1]
                    ]
              let (tyId, st2) = emitTypeFromLayout st1 vecLayout
              let (resId, st3) = freshId st2
              let fs2 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) elems)) fs1
              (st4, fs3, rest) <- go (colIx + 1) st3 fs2
              Right (st4, fs3, Value vecLayout resId : rest)
    buildColumns vecLayout vals st' fs' =
      case splitAt rows vals of
        ([], _) -> Right (st', fs', [])
        (col, rest) -> do
          let (tyId, st1) = emitTypeFromLayout st' vecLayout
          let (resId, st2) = freshId st1
          let fs1 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) col)) fs'
          (st3, fs2, colsVals) <- buildColumns vecLayout rest st2 fs1
          Right (st3, fs2, Value vecLayout resId : colsVals)

emitStructCtor :: Text -> TypeLayout -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitStructCtor name layout args st fs =
  case layout of
    TLStruct _ fields _ _ -> do
      case args of
        [] -> do
          (st1, v) <- emitZeroConstValue layout st
          Right (st1, fs, v)
        _ -> do
          when (length args /= length fields) $
            Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
          (st1, fs1, vals) <- emitExprList st fs args
          (st2, fs2, vals') <- coerceArgsToLayouts vals (map (.flType) fields) st1 fs1
          let (tyId, st3) = emitTypeFromLayout st2 layout
          let (resId, st4) = freshId st3
          let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) vals')) fs2
          Right (st4, fs3, Value layout resId)
    _ -> Left (CompileError ("unsupported constructor: " <> textToString name) Nothing Nothing)

emitArrayCtor :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitArrayCtor args st fs =
  case args of
    [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
    _ -> do
      (st1, fs1, vals) <- emitExprList st fs args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        _ -> do
          let firstLayout = pickBaseLayout st1 vals
          (st2, fs2, vals') <- coerceValuesToLayout firstLayout vals st1 fs1
          when (containsResource firstLayout) $
            Left (CompileError "arrays of resources are not supported" Nothing Nothing)
          when (containsAtomic firstLayout) $
            Left (CompileError "arrays of atomic types are not supported" Nothing Nothing)
          let elemAlign = layoutAlign firstLayout
          let elemSize = layoutSize firstLayout
          let stride = roundUp elemSize elemAlign
          let total = stride * fromIntegral (length vals')
          let layout = TLArray (Just (length vals)) stride firstLayout elemAlign total
          let (tyId, st3) = emitTypeFromLayout st2 layout
          let (resId, st4) = freshId st3
          let fs3 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map (.valId) vals')) fs2
          Right (st4, fs3, Value layout resId)

emitScalarCtor :: Scalar -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitScalarCtor scalar args st fs =
  case args of
    [] -> do
      (st1, v) <- emitZeroConstScalar scalar st
      Right (st1, fs, v)
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case val.valType of
        TLScalar s _ _ | s == scalar -> Right (st1, fs1, val)
        TLScalar s _ _ -> emitScalarConvert s scalar val st1 fs1
        _ -> Left (CompileError "scalar cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar cast requires a single argument" Nothing Nothing)

emitScalarConvert :: Scalar -> Scalar -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitScalarConvert fromScalar toScalarTy val st fs = do
  opcode <- case (fromScalar, toScalarTy) of
    (U32, F32) -> Right opConvertUToF
    (I32, F32) -> Right opConvertSToF
    (U32, F16) -> Right opConvertUToF
    (I32, F16) -> Right opConvertSToF
    (F32, U32) -> Right opConvertFToU
    (F32, I32) -> Right opConvertFToS
    (F16, U32) -> Right opConvertFToU
    (F16, I32) -> Right opConvertFToS
    (F16, F32) -> Right opFConvert
    (F32, F16) -> Right opFConvert
    (U32, I32) -> Right opBitcast
    (I32, U32) -> Right opBitcast
    _ -> Left (CompileError "unsupported scalar conversion" Nothing Nothing)
  let (a, sz) = scalarLayout toScalarTy
  let layout = TLScalar toScalarTy a sz
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opcode [tyId, resId, val.valId]) fs
  Right (st2, fs1, Value layout resId)

isConstLiteral :: GenState -> Value -> Bool
isConstLiteral st val = isJust (lookupConstKeyById st (val.valId))

pickBaseLayout :: GenState -> [Value] -> TypeLayout
pickBaseLayout st vals =
  case dropWhile (isConstLiteral st) vals of
    (v:_) -> v.valType
    [] ->
      case vals of
        (v:_) -> v.valType
        [] ->
          let (a, sz) = scalarLayout F32
          in TLScalar F32 a sz

coerceValuesToLayout :: TypeLayout -> [Value] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
coerceValuesToLayout target vals st fs = go st fs [] vals
  where
    go st' fs' acc [] = Right (st', fs', reverse acc)
    go st' fs' acc (v:vs) = do
      (st1, fs1, v') <- coerceValueToLayout target v st' fs'
      go st1 fs1 (v':acc) vs

flattenValuesToScalars :: [Value] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
flattenValuesToScalars vals st fs = go st fs [] vals
  where
    go st' fs' acc [] = Right (st', fs', reverse acc)
    go st' fs' acc (v:vs) =
      case v.valType of
        TLScalar {} -> go st' fs' (v:acc) vs
        TLVector n _ _ _ -> do
          (st1, fs1, comps) <- extractComponents 0 (fromIntegral n) v st' fs'
          go st1 fs1 (reverse comps <> acc) vs
        _ -> Left (CompileError "vector constructor arguments must be scalars or vectors" Nothing Nothing)

    extractComponents :: Int -> Int -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
    extractComponents idx count vecVal st' fs'
      | idx >= count = Right (st', fs', [])
      | otherwise = do
          (st1, fs1, comp) <- emitVectorComponent vecVal (fromIntegral idx) st' fs'
          (st2, fs2, rest) <- extractComponents (idx + 1) count vecVal st1 fs1
          Right (st2, fs2, comp : rest)

coerceValuesToScalar :: Scalar -> [Value] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
coerceValuesToScalar target vals st fs = go st fs [] vals
  where
    go st' fs' acc [] = Right (st', fs', reverse acc)
    go st' fs' acc (v:vs) =
      case v.valType of
        TLScalar s _ _
          | s == target -> go st' fs' (v:acc) vs
          | otherwise -> do
              (st1, fs1, v') <- emitScalarConvert s target v st' fs'
              go st1 fs1 (v':acc) vs
        _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)

coerceArgsToLayouts :: [Value] -> [TypeLayout] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, [Value])
coerceArgsToLayouts vals tys st fs = go st fs [] vals tys
  where
    go st' fs' acc [] [] = Right (st', fs', reverse acc)
    go st' fs' acc (v:vs) (t:ts) = do
      (st1, fs1, v') <- coerceValueToLayout t v st' fs'
      go st1 fs1 (v':acc) vs ts
    go _ _ _ _ _ = Left (CompileError "function arity mismatch" Nothing Nothing)

coerceValueToLayout :: TypeLayout -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
coerceValueToLayout target val st fs
  | val.valType == target = Right (st, fs, val)
  | otherwise =
      case (target, val.valType) of
        (TLPointer {}, TLPointer {}) ->
          case ensureTypeMatch target (val.valType) of
            Right _ -> Right (st, fs, val)
            Left _ -> Left (CompileError "type mismatch" Nothing Nothing)
        (TLScalar targetScalar _ _, TLScalar {}) ->
          case lookupConstKeyById st (val.valId) of
            Nothing -> Left (CompileError "implicit conversion requires a constant literal" Nothing Nothing)
            Just key -> do
              key' <- convertConstKey key targetScalar
              let (cid, st1) = emitConstFromKey st key'
              let (a, sz) = scalarLayout targetScalar
              let layout = TLScalar targetScalar a sz
              Right (st1, fs, Value layout cid)
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceBinaryOperands :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value, Value, TypeLayout)
coerceBinaryOperands lval rval st fs
  | lval.valType == rval.valType = Right (st, fs, lval, rval, lval.valType)
  | otherwise =
      case (lval.valType, rval.valType) of
        (TLScalar {}, TLScalar {}) ->
          let lConst = lookupConstKeyById st (lval.valId)
              rConst = lookupConstKeyById st (rval.valId)
          in case (lConst, rConst) of
            (_, Just _) -> do
              (st1, fs1, rval') <- coerceValueToLayout (lval.valType) rval st fs
              Right (st1, fs1, lval, rval', lval.valType)
            (Just _, _) -> do
              (st1, fs1, lval') <- coerceValueToLayout (rval.valType) lval st fs
              Right (st1, fs1, lval', rval, rval.valType)
            _ -> Left (CompileError "type mismatch" Nothing Nothing)
        (TLVector n scalar _ _, TLScalar s _ _) -> do
          (st1, fs1, scalarVal) <-
            if s == scalar
              then Right (st, fs, rval)
              else emitScalarConvert s scalar rval st fs
          let (a, sz) = vectorLayout scalar n
          let layout = TLVector n scalar a sz
          (st2, fs2, rvalVec) <- emitSplatVector layout (scalarVal.valId) st1 fs1
          Right (st2, fs2, lval, rvalVec, layout)
        (TLScalar s _ _, TLVector n scalar _ _) -> do
          (st1, fs1, scalarVal) <-
            if s == scalar
              then Right (st, fs, lval)
              else emitScalarConvert s scalar lval st fs
          let (a, sz) = vectorLayout scalar n
          let layout = TLVector n scalar a sz
          (st2, fs2, lvalVec) <- emitSplatVector layout (scalarVal.valId) st1 fs1
          Right (st2, fs2, lvalVec, rval, layout)
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceConstValueToLayout :: TypeLayout -> Value -> GenState -> Either CompileError (GenState, Value)
coerceConstValueToLayout target val st
  | val.valType == target = Right (st, val)
  | otherwise =
      case (target, val.valType) of
        (TLScalar targetScalar _ _, TLScalar {}) ->
          case lookupConstKeyById st (val.valId) of
            Nothing -> Left (CompileError "implicit conversion requires a constant literal" Nothing Nothing)
            Just key -> do
              key' <- convertConstKey key targetScalar
              let (cid, st1) = emitConstFromKey st key'
              let (a, sz) = scalarLayout targetScalar
              let layout = TLScalar targetScalar a sz
              Right (st1, Value layout cid)
        _ -> Left (CompileError "type mismatch" Nothing Nothing)

coerceConstValuesToLayout :: TypeLayout -> [Value] -> GenState -> Either CompileError (GenState, [Value])
coerceConstValuesToLayout target vals st = go st [] vals
  where
    go st' acc [] = Right (st', reverse acc)
    go st' acc (v:vs) = do
      (st1, v') <- coerceConstValueToLayout target v st'
      go st1 (v':acc) vs

flattenConstScalars :: GenState -> [Value] -> Either CompileError [Value]
flattenConstScalars st vals = fmap concat (mapM flatten vals)
  where
    flatten val =
      case val.valType of
        TLScalar {} -> Right [val]
        TLVector n scalar _ _ ->
          case Map.lookup val.valId st.gsConstComposites of
            Just (layout, parts) ->
              case layout of
                TLVector n' scalar' _ _
                  | n == n' && scalar == scalar' ->
                      Right (map (Value (scalarLayoutType scalar)) parts)
                _ -> Left (CompileError "vector constructor expects vector constants" Nothing Nothing)
            Nothing -> Left (CompileError "vector constructor expects constant vectors" Nothing Nothing)
        _ -> Left (CompileError "vector constructor arguments must be scalars or vectors" Nothing Nothing)

    scalarLayoutType s =
      let (a, sz) = scalarLayout s
      in TLScalar s a sz

coerceConstArgsToLayouts :: [Value] -> [TypeLayout] -> GenState -> Either CompileError (GenState, [Value])
coerceConstArgsToLayouts vals tys st = go st [] vals tys
  where
    go st' acc [] [] = Right (st', reverse acc)
    go st' acc (v:vs) (t:ts) = do
      (st1, v') <- coerceConstValueToLayout t v st'
      go st1 (v':acc) vs ts
    go _ _ _ _ = Left (CompileError "constructor arity mismatch" Nothing Nothing)

emitVectorComponent :: Value -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVectorComponent vecVal idx st fs =
  case vecVal.valType of
    TLVector n scalar _ _ -> do
      when (idx >= fromIntegral n) $
        Left (CompileError "vector component out of range" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let layout = TLScalar scalar a sz
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opCompositeExtract [tyId, resId, vecVal.valId, idx]) fs
      Right (st2, fs1, Value layout resId)
    _ -> Left (CompileError "expected vector value" Nothing Nothing)

emitVec3FromVec2Scalar :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVec3FromVec2Scalar vecVal scalarVal st fs =
  case vecVal.valType of
    TLVector 2 scalar _ _ -> do
      (st1, fs1, xVal) <- emitVectorComponent vecVal 0 st fs
      (st2, fs2, yVal) <- emitVectorComponent vecVal 1 st1 fs1
      (st3, fs3, scalarVal') <- case scalarVal.valType of
        TLScalar s _ _ | s == scalar -> Right (st2, fs2, scalarVal)
        TLScalar s _ _ -> emitScalarConvert s scalar scalarVal st2 fs2
        _ -> Left (CompileError "expected scalar value" Nothing Nothing)
      let (a, sz) = vectorLayout scalar 3
      let layout = TLVector 3 scalar a sz
      let (tyId, st4) = emitTypeFromLayout st3 layout
      let (resId, st5) = freshId st4
      let fs4 = addFuncInstr (Instr opCompositeConstruct [tyId, resId, xVal.valId, yVal.valId, scalarVal'.valId]) fs3
      Right (st5, fs4, Value layout resId)
    _ -> Left (CompileError "expected vec2 value" Nothing Nothing)

emitVec2FromScalarScalar :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVec2FromScalarScalar scalarVal arrayVal st fs =
  case scalarVal.valType of
    TLScalar scalar _ _ -> do
      (st1, fs1, arrayVal') <- case arrayVal.valType of
        TLScalar s _ _ | s == scalar -> Right (st, fs, arrayVal)
        TLScalar s _ _ -> emitScalarConvert s scalar arrayVal st fs
        _ -> Left (CompileError "expected scalar value" Nothing Nothing)
      let (a, sz) = vectorLayout scalar 2
      let layout = TLVector 2 scalar a sz
      let (tyId, st2) = emitTypeFromLayout st1 layout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opCompositeConstruct [tyId, resId, scalarVal.valId, arrayVal'.valId]) fs1
      Right (st3, fs2, Value layout resId)
    _ -> Left (CompileError "expected scalar value" Nothing Nothing)

emitVec4FromVec3Scalar :: Value -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVec4FromVec3Scalar vecVal scalarVal st fs =
  case vecVal.valType of
    TLVector 3 scalar _ _ -> do
      (st1, fs1, xVal) <- emitVectorComponent vecVal 0 st fs
      (st2, fs2, yVal) <- emitVectorComponent vecVal 1 st1 fs1
      (st3, fs3, zVal) <- emitVectorComponent vecVal 2 st2 fs2
      (st4, fs4, scalarVal') <- case scalarVal.valType of
        TLScalar s _ _ | s == scalar -> Right (st3, fs3, scalarVal)
        TLScalar s _ _ -> emitScalarConvert s scalar scalarVal st3 fs3
        _ -> Left (CompileError "expected scalar value" Nothing Nothing)
      let (a, sz) = vectorLayout scalar 4
      let layout = TLVector 4 scalar a sz
      let (tyId, st5) = emitTypeFromLayout st4 layout
      let (resId, st6) = freshId st5
      let fs5 = addFuncInstr (Instr opCompositeConstruct [tyId, resId, xVal.valId, yVal.valId, zVal.valId, scalarVal'.valId]) fs4
      Right (st6, fs5, Value layout resId)
    _ -> Left (CompileError "expected vec3 value" Nothing Nothing)

emitSamplerExpr :: TypeLayout -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSamplerExpr expected expr st fs =
  if st.gsSamplerMode == SamplerCombined
    then Right (st, fs, Value expected 0)
    else emitExpr st fs expr

emitTextureSample :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSample args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) st fs
    _ -> Left (CompileError "textureSample expects (texture, sampler, coords[, array_index])" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSampler samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      case sampVal.valType of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSample expects a sampler binding" Nothing Nothing)
      case (texVal.valType, mArray) of
        (TLTexture1D s, Nothing) -> do
          ensureFloatScalar (coordVal.valType)
          sampleColorCoord s coordVal texVal sampVal st3 fs3
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal st3 fs3
        (TLTexture2D s, Nothing) -> sampleColor s coordVal texVal sampVal st3 fs3
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal st3 fs3
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal st3 fs3
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal st3 fs3
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal st3 fs3
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal st3 fs3
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal st3 fs3
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal st3 fs3
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal st3 fs3
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSample for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSample for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSample for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSample for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSample for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSample is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSample is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSample expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 =
      if st0.gsSamplerMode == SamplerCombined
        then Right (st0, fs0, texVal.valId)
        else do
          let (imageTy, st1) = emitTypeFromLayout st0 (texVal.valType)
          let (sampledTy, st2) = emitSampledImageType imageTy st1
          let (sampledId, st3) = freshId st2
          let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs0
          Right (st3, fs1, sampledId)

    sampleColorCoord scalar coordVal texVal sampVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, coordVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor3D scalar coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleColorCoord scalar coordVal texVal sampVal st0 fs0

    sampleColor scalar coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleColorCoord scalar coordVal texVal sampVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatScalar (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoord scalar coordVal' texVal sampVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoord scalar coordVal' texVal sampVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoord scalar coordVal' texVal sampVal st2 fs2

    sampleDepthCoord coordVal texVal sampVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, coordVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleDepthCoord coordVal texVal sampVal st0 fs0

    sampleDepthCube coordVal texVal sampVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleDepthCoord coordVal texVal sampVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoord coordVal' texVal sampVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoord coordVal' texVal sampVal st2 fs2

emitTextureSampleCompare :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleCompare args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, depthExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr Nothing depthExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, depthExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr (Just arrayExpr) depthExpr st fs
    _ -> Left (CompileError "textureSampleCompare expects (texture, sampler_comparison, coords, depth_ref) or (texture, sampler_comparison, coords, array_index, depth_ref)" Nothing Nothing)
  where
    emitCompareSample texExpr samplerExpr coordExpr mArray depthExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSamplerComparison samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, depthVal) <- emitExpr st3 fs3 depthExpr
      case sampVal.valType of
        TLSamplerComparison -> Right ()
        _ -> Left (CompileError "textureSampleCompare expects a sampler_comparison binding" Nothing Nothing)
      ensureFloatScalar (depthVal.valType)
      (st5, fs5, coordVal') <-
        case (texVal.valType, mArray) of
          (TLTextureDepth2D, Nothing) -> do
            ensureFloatVec2 (coordVal.valType)
            Right (st4, fs4, coordVal)
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureFloatVec2 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCube, Nothing) -> do
            ensureFloatVec3 (coordVal.valType)
            Right (st4, fs4, coordVal)
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureFloatVec3 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureSampleCompare for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureSampleCompare for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureSampleCompare expects a depth texture binding" Nothing Nothing)
      (st6, fs6, depthVal') <-
        case depthVal.valType of
          TLScalar F32 _ _ -> Right (st5, fs5, depthVal)
          TLScalar F16 _ _ -> emitScalarConvert F16 F32 depthVal st5 fs5
          _ -> Left (CompileError "depth reference must be f32 or f16" Nothing Nothing)
      (st7, fs7, sampledId) <-
        if st6.gsSamplerMode == SamplerCombined
          then Right (st6, fs6, texVal.valId)
          else do
            let (imageTy, st7') = emitTypeFromLayout st6 (texVal.valType)
            let (sampledTy, st8) = emitSampledImageType imageTy st7'
            let (sampledId, st9) = freshId st8
            let fs7' = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs6
            Right (st9, fs7', sampledId)
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st10) = emitTypeFromLayout st7 outLayout
      let (resId, st11) = freshId st10
      let fs8 = addFuncInstr (Instr opImageSampleDrefImplicitLod [outTy, resId, sampledId, coordVal'.valId, depthVal'.valId]) fs7
      Right (st11, fs8, Value outLayout resId)

emitTextureSampleLevel :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleLevel args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, levelExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing levelExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, levelExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) levelExpr st fs
    _ -> Left (CompileError "textureSampleLevel expects (texture, sampler, coords[, array_index], level)" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray levelExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSampler samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, levelVal) <- emitExpr st3 fs3 levelExpr
      case sampVal.valType of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSampleLevel expects a sampler binding" Nothing Nothing)
      (st5, fs5, lodVal) <- case levelVal.valType of
        TLScalar F32 _ _ -> Right (st4, fs4, levelVal)
        TLScalar F16 _ _ -> emitScalarConvert F16 F32 levelVal st4 fs4
        _ -> Left (CompileError "textureSampleLevel requires a float level" Nothing Nothing)
      case (texVal.valType, mArray) of
        (TLTexture1D s, Nothing) -> do
          ensureFloatScalar (coordVal.valType)
          sampleColorCoordLod s coordVal texVal sampVal lodVal st5 fs5
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTexture2D s, Nothing) -> sampleColor2D s coordVal texVal sampVal lodVal st5 fs5
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal lodVal st5 fs5
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal lodVal st5 fs5
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal lodVal st5 fs5
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal lodVal st5 fs5
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal lodVal st5 fs5
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSampleLevel for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSampleLevel is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSampleLevel is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSampleLevel expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 =
      if st0.gsSamplerMode == SamplerCombined
        then Right (st0, fs0, texVal.valId)
        else do
          let (imageTy, st1) = emitTypeFromLayout st0 (texVal.valType)
          let (sampledTy, st2) = emitSampledImageType imageTy st1
          let (sampledId, st3) = freshId st2
          let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs0
          Right (st3, fs1, sampledId)

    sampleColorCoordLod scalar coordVal texVal sampVal lodVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, coordVal.valId, imageOperandsLod, lodVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor2D scalar coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleColorCoordLod scalar coordVal texVal sampVal lodVal st0 fs0

    sampleColor3D scalar coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleColorCoordLod scalar coordVal texVal sampVal lodVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatScalar (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoordLod scalar coordVal' texVal sampVal lodVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoordLod scalar coordVal' texVal sampVal lodVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoordLod scalar coordVal' texVal sampVal lodVal st2 fs2

    sampleDepthCoordLod coordVal texVal sampVal lodVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, coordVal.valId, imageOperandsLod, lodVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleDepthCoordLod coordVal texVal sampVal lodVal st0 fs0

    sampleDepthCube coordVal texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleDepthCoordLod coordVal texVal sampVal lodVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordLod coordVal' texVal sampVal lodVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal lodVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordLod coordVal' texVal sampVal lodVal st2 fs2

emitTextureSampleBias :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleBias args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, biasExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing biasExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, biasExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) biasExpr st fs
    _ -> Left (CompileError "textureSampleBias expects (texture, sampler, coords[, array_index], bias)" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray biasExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSampler samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, biasVal) <- emitExpr st3 fs3 biasExpr
      case sampVal.valType of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSampleBias expects a sampler binding" Nothing Nothing)
      (st5, fs5, biasVal') <- case biasVal.valType of
        TLScalar F32 _ _ -> Right (st4, fs4, biasVal)
        TLScalar F16 _ _ -> emitScalarConvert F16 F32 biasVal st4 fs4
        _ -> Left (CompileError "textureSampleBias requires a float bias" Nothing Nothing)
      case (texVal.valType, mArray) of
        (TLTexture1D s, Nothing) -> do
          ensureFloatScalar (coordVal.valType)
          sampleColorCoordBias s coordVal texVal sampVal biasVal' st5 fs5
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTexture2D s, Nothing) -> sampleColor2D s coordVal texVal sampVal biasVal' st5 fs5
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal biasVal' st5 fs5
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal biasVal' st5 fs5
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSampleBias for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSampleBias for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSampleBias for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSampleBias for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSampleBias for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSampleBias is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSampleBias is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSampleBias expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 =
      if st0.gsSamplerMode == SamplerCombined
        then Right (st0, fs0, texVal.valId)
        else do
          let (imageTy, st1) = emitTypeFromLayout st0 (texVal.valType)
          let (sampledTy, st2) = emitSampledImageType imageTy st1
          let (sampledId, st3) = freshId st2
          let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs0
          Right (st3, fs1, sampledId)

    sampleColorCoordBias scalar coordVal texVal sampVal biasVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, coordVal.valId, imageOperandsBias, biasVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor2D scalar coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleColorCoordBias scalar coordVal texVal sampVal biasVal st0 fs0

    sampleColor3D scalar coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleColorCoordBias scalar coordVal texVal sampVal biasVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatScalar (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoordBias scalar coordVal' texVal sampVal biasVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoordBias scalar coordVal' texVal sampVal biasVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoordBias scalar coordVal' texVal sampVal biasVal st2 fs2

    sampleDepthCoordBias coordVal texVal sampVal biasVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, coordVal.valId, imageOperandsBias, biasVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleDepthCoordBias coordVal texVal sampVal biasVal st0 fs0

    sampleDepthCube coordVal texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleDepthCoordBias coordVal texVal sampVal biasVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordBias coordVal' texVal sampVal biasVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal biasVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordBias coordVal' texVal sampVal biasVal st2 fs2

emitTextureSampleGrad :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleGrad args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, ddxExpr, ddyExpr] ->
      emitSample texExpr samplerExpr coordExpr Nothing ddxExpr ddyExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, ddxExpr, ddyExpr] ->
      emitSample texExpr samplerExpr coordExpr (Just arrayExpr) ddxExpr ddyExpr st fs
    _ -> Left (CompileError "textureSampleGrad expects (texture, sampler, coords[, array_index], ddx, ddy)" Nothing Nothing)
  where
    emitSample texExpr samplerExpr coordExpr mArray ddxExpr ddyExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSampler samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, ddxVal) <- emitExpr st3 fs3 ddxExpr
      (st5, fs5, ddyVal) <- emitExpr st4 fs4 ddyExpr
      case sampVal.valType of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSampleGrad expects a sampler binding" Nothing Nothing)
      ensureFloatNumeric (coordVal.valType)
      ensureTypeMatch (coordVal.valType) (ddxVal.valType)
      ensureTypeMatch (coordVal.valType) (ddyVal.valType)
      case (texVal.valType, mArray) of
        (TLTexture1D s, Nothing) ->
          sampleColorCoordGrad s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture1DArray s, Just arrayExpr) ->
          sampleColor1DArray s coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture2D s, Nothing) -> sampleColor2D s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture2DArray s, Just arrayExpr) -> sampleColorArray s coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture3D s, Nothing) -> sampleColor3D s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureCube s, Nothing) -> sampleColor3D s coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureCubeArray s, Just arrayExpr) ->
          sampleColorCubeArray s coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepth2D, Nothing) -> sampleDepth2D coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepth2DArray, Just arrayExpr) -> sampleDepthArray2D coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepthCube, Nothing) -> sampleDepthCube coordVal texVal sampVal ddxVal ddyVal st5 fs5
        (TLTextureDepthCubeArray, Just arrayExpr) -> sampleDepthCubeArray coordVal arrayExpr texVal sampVal ddxVal ddyVal st5 fs5
        (TLTexture1DArray _, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_1d_array requires an array index" Nothing Nothing)
        (TLTexture2DArray _, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_2d_array requires an array index" Nothing Nothing)
        (TLTextureCubeArray _, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_cube_array requires an array index" Nothing Nothing)
        (TLTextureDepth2DArray, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_depth_2d_array requires an array index" Nothing Nothing)
        (TLTextureDepthCubeArray, Nothing) ->
          Left (CompileError "textureSampleGrad for texture_depth_cube_array requires an array index" Nothing Nothing)
        (TLTextureMultisampled2D _, _) ->
          Left (CompileError "textureSampleGrad is not supported for multisampled textures" Nothing Nothing)
        (TLTextureDepthMultisampled2D, _) ->
          Left (CompileError "textureSampleGrad is not supported for multisampled textures" Nothing Nothing)
        _ -> Left (CompileError "textureSampleGrad expects a sampled texture binding" Nothing Nothing)

    buildSampledImage texVal sampVal st0 fs0 =
      if st0.gsSamplerMode == SamplerCombined
        then Right (st0, fs0, texVal.valId)
        else do
          let (imageTy, st1) = emitTypeFromLayout st0 (texVal.valType)
          let (sampledTy, st2) = emitSampledImageType imageTy st1
          let (sampledId, st3) = freshId st2
          let fs1 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs0
          Right (st3, fs1, sampledId)

    sampleColorCoordGrad scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, coordVal.valId, imageOperandsGrad, ddxVal.valId, ddyVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleColor2D scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleColorCoordGrad scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleColor3D scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleColorCoordGrad scalar coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleColor1DArray scalar coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatScalar (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal st1 fs1
      sampleColorCoordGrad scalar coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleColorCubeArray scalar coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleColorCoordGrad scalar coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleColorArray scalar coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleColorCoordGrad scalar coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleDepthCoordGrad coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      (st1, fs1, sampledId) <- buildSampledImage texVal sampVal st0 fs0
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st2) = emitTypeFromLayout st1 outLayout
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opImageSampleExplicitLod [outTy, resId, sampledId, coordVal.valId, imageOperandsGrad, ddxVal.valId, ddyVal.valId]) fs1
      Right (st3, fs2, Value outLayout resId)

    sampleDepth2D coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      sampleDepthCoordGrad coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleDepthCube coordVal texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      sampleDepthCoordGrad coordVal texVal sampVal ddxVal ddyVal st0 fs0

    sampleDepthArray2D coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec2 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordGrad coordVal' texVal sampVal ddxVal ddyVal st2 fs2

    sampleDepthCubeArray coordVal arrayExpr texVal sampVal ddxVal ddyVal st0 fs0 = do
      ensureFloatVec3 (coordVal.valType)
      (st1, fs1, arrayVal) <- emitExpr st0 fs0 arrayExpr
      ensureIndexType (arrayVal.valType)
      (st2, fs2, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal st1 fs1
      sampleDepthCoordGrad coordVal' texVal sampVal ddxVal ddyVal st2 fs2

emitTextureSampleCompareLevel :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSampleCompareLevel args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, depthExpr, levelExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr Nothing depthExpr levelExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, depthExpr, levelExpr] ->
      emitCompareSample texExpr samplerExpr coordExpr (Just arrayExpr) depthExpr levelExpr st fs
    _ -> Left (CompileError "textureSampleCompareLevel expects (texture, sampler_comparison, coords[, array_index], depth_ref, level)" Nothing Nothing)
  where
    emitCompareSample texExpr samplerExpr coordExpr mArray depthExpr levelExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSamplerComparison samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, depthVal) <- emitExpr st3 fs3 depthExpr
      (st5, fs5, levelVal) <- emitExpr st4 fs4 levelExpr
      case sampVal.valType of
        TLSamplerComparison -> Right ()
        _ -> Left (CompileError "textureSampleCompareLevel expects a sampler_comparison binding" Nothing Nothing)
      ensureFloatScalar (depthVal.valType)
      (st6, fs6, lodVal) <- case levelVal.valType of
        TLScalar F32 _ _ -> Right (st5, fs5, levelVal)
        TLScalar F16 _ _ -> emitScalarConvert F16 F32 levelVal st5 fs5
        _ -> Left (CompileError "textureSampleCompareLevel requires a float level" Nothing Nothing)
      (st7, fs7, coordVal') <-
        case (texVal.valType, mArray) of
          (TLTextureDepth2D, Nothing) -> do
            ensureFloatVec2 (coordVal.valType)
            Right (st6, fs6, coordVal)
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureFloatVec2 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st6 fs6 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCube, Nothing) -> do
            ensureFloatVec3 (coordVal.valType)
            Right (st6, fs6, coordVal)
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureFloatVec3 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st6 fs6 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureSampleCompareLevel for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureSampleCompareLevel for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureSampleCompareLevel expects a depth texture binding" Nothing Nothing)
      (st8, fs8, depthVal') <-
        case depthVal.valType of
          TLScalar F32 _ _ -> Right (st7, fs7, depthVal)
          TLScalar F16 _ _ -> emitScalarConvert F16 F32 depthVal st7 fs7
          _ -> Left (CompileError "depth reference must be f32 or f16" Nothing Nothing)
      (st9, fs9, sampledId) <-
        if st8.gsSamplerMode == SamplerCombined
          then Right (st8, fs8, texVal.valId)
          else do
            let (imageTy, st9') = emitTypeFromLayout st8 (texVal.valType)
            let (sampledTy, st10) = emitSampledImageType imageTy st9'
            let (sampledId, st11) = freshId st10
            let fs9' = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs8
            Right (st11, fs9', sampledId)
      let (a, sz) = scalarLayout F32
      let outLayout = TLScalar F32 a sz
      let (outTy, st12) = emitTypeFromLayout st9 outLayout
      let (resId, st13) = freshId st12
      let fs10 = addFuncInstr (Instr opImageSampleDrefExplicitLod [outTy, resId, sampledId, coordVal'.valId, depthVal'.valId, imageOperandsLod, lodVal.valId]) fs9
      Right (st13, fs10, Value outLayout resId)

emitTextureGather :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureGather args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr] ->
      emitGather texExpr samplerExpr coordExpr Nothing Nothing st fs
    [texExpr, samplerExpr, coordExpr, arg4] ->
      emitGather texExpr samplerExpr coordExpr (Just arg4) Nothing st fs
    [texExpr, samplerExpr, coordExpr, arg4, arg5] ->
      emitGather texExpr samplerExpr coordExpr (Just arg4) (Just arg5) st fs
    _ -> Left (CompileError "textureGather expects (texture, sampler, coords[, array_index|component][, component])" Nothing Nothing)
  where
    emitGather texExpr samplerExpr coordExpr mArrayOrComp mComp st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSampler samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      case sampVal.valType of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureGather expects a sampler binding" Nothing Nothing)
      let isArray = case texVal.valType of
            TLTexture2DArray _ -> True
            TLTextureCubeArray _ -> True
            _ -> False
      (arrayExpr', compExpr') <-
        case (isArray, mArrayOrComp, mComp) of
          (False, Nothing, Nothing) -> Right (Nothing, Nothing)
          (False, Just compExpr, Nothing) -> Right (Nothing, Just compExpr)
          (False, Nothing, Just _) ->
            Left (CompileError "textureGather component must be provided as the fourth argument" Nothing Nothing)
          (False, Just _, Just _) ->
            Left (CompileError "textureGather only accepts one component argument" Nothing Nothing)
          (True, Just arrayExpr, Nothing) -> Right (Just arrayExpr, Nothing)
          (True, Just arrayExpr, Just compExpr) -> Right (Just arrayExpr, Just compExpr)
          (True, Nothing, _) ->
            Left (CompileError "textureGather for array textures requires an array index" Nothing Nothing)
      (st4, fs4, coordValFinal, scalar) <-
        case (texVal.valType, arrayExpr') of
          (TLTexture2D s, Nothing) -> do
            ensureFloatVec2 (coordVal.valType)
            Right (st3, fs3, coordVal, s)
          (TLTexture2DArray s, Just arrayExpr) -> do
            ensureFloatVec2 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st3 fs3 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal', s)
          (TLTextureCube s, Nothing) -> do
            ensureFloatVec3 (coordVal.valType)
            Right (st3, fs3, coordVal, s)
          (TLTextureCubeArray s, Just arrayExpr) -> do
            ensureFloatVec3 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st3 fs3 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal', s)
          _ -> Left (CompileError "textureGather expects a 2d or cube sampled texture" Nothing Nothing)
      (st6, fs6, compVal) <- case compExpr' of
        Nothing ->
          let (cid, st') = emitConstU32 st4 0
              layout = uncurry (TLScalar U32) (scalarLayout U32)
          in Right (st', fs4, Value layout cid)
        Just compExpr -> do
          (stA, fsA, compVal0) <- emitExpr st4 fs4 compExpr
          ensureIndexType (compVal0.valType)
          case compVal0.valType of
            TLScalar U32 _ _ -> Right (stA, fsA, compVal0)
            TLScalar I32 _ _ -> emitScalarConvert I32 U32 compVal0 stA fsA
            _ -> Left (CompileError "textureGather component must be i32 or u32" Nothing Nothing)
      (st7, fs7, sampledId) <-
        if st6.gsSamplerMode == SamplerCombined
          then Right (st6, fs6, texVal.valId)
          else do
            let (imageTy, st7') = emitTypeFromLayout st6 (texVal.valType)
            let (sampledTy, st8) = emitSampledImageType imageTy st7'
            let (sampledId, st9) = freshId st8
            let fs7' = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs6
            Right (st9, fs7', sampledId)
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st10) = emitTypeFromLayout st7 outLayout
      let (resId, st11) = freshId st10
      let fs8 = addFuncInstr (Instr opImageGather [outTy, resId, sampledId, coordValFinal.valId, compVal.valId]) fs7
      Right (st11, fs8, Value outLayout resId)

emitTextureGatherCompare :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureGatherCompare args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr, depthExpr] ->
      emitGather texExpr samplerExpr coordExpr Nothing depthExpr st fs
    [texExpr, samplerExpr, coordExpr, arrayExpr, depthExpr] ->
      emitGather texExpr samplerExpr coordExpr (Just arrayExpr) depthExpr st fs
    _ -> Left (CompileError "textureGatherCompare expects (texture, sampler_comparison, coords[, array_index], depth_ref)" Nothing Nothing)
  where
    emitGather texExpr samplerExpr coordExpr mArray depthExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, sampVal) <- emitSamplerExpr TLSamplerComparison samplerExpr st1 fs1
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      (st4, fs4, depthVal) <- emitExpr st3 fs3 depthExpr
      case sampVal.valType of
        TLSamplerComparison -> Right ()
        _ -> Left (CompileError "textureGatherCompare expects a sampler_comparison binding" Nothing Nothing)
      ensureFloatScalar (depthVal.valType)
      (st5, fs5, coordVal') <-
        case (texVal.valType, mArray) of
          (TLTextureDepth2D, Nothing) -> do
            ensureFloatVec2 (coordVal.valType)
            Right (st4, fs4, coordVal)
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureFloatVec2 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCube, Nothing) -> do
            ensureFloatVec3 (coordVal.valType)
            Right (st4, fs4, coordVal)
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureFloatVec3 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st4 fs4 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureGatherCompare for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureGatherCompare for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureGatherCompare expects a depth texture binding" Nothing Nothing)
      (st6, fs6, depthVal') <-
        case depthVal.valType of
          TLScalar F32 _ _ -> Right (st5, fs5, depthVal)
          TLScalar F16 _ _ -> emitScalarConvert F16 F32 depthVal st5 fs5
          _ -> Left (CompileError "depth reference must be f32 or f16" Nothing Nothing)
      (st7, fs7, sampledId) <-
        if st6.gsSamplerMode == SamplerCombined
          then Right (st6, fs6, texVal.valId)
          else do
            let (imageTy, st7') = emitTypeFromLayout st6 (texVal.valType)
            let (sampledTy, st8) = emitSampledImageType imageTy st7'
            let (sampledId, st9) = freshId st8
            let fs7' = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, texVal.valId, sampVal.valId]) fs6
            Right (st9, fs7', sampledId)
      let (align, size) = vectorLayout F32 4
      let outLayout = TLVector 4 F32 align size
      let (outTy, st10) = emitTypeFromLayout st7 outLayout
      let (resId, st11) = freshId st10
      let fs8 = addFuncInstr (Instr opImageDrefGather [outTy, resId, sampledId, coordVal'.valId, depthVal'.valId]) fs7
      Right (st11, fs8, Value outLayout resId)

emitTextureDimensions :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureDimensions args st fs =
  case args of
    [texExpr] -> emitDim texExpr Nothing st fs
    [texExpr, levelExpr] -> emitDim texExpr (Just levelExpr) st fs
    _ -> Left (CompileError "textureDimensions expects (texture[, level])" Nothing Nothing)
  where
    emitDim texExpr mLevel st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (stImage, fsImage, texId) <- emitImageFromTextureValue st1 fs1 texVal
      (st2, fs2, mLevelVal) <- case mLevel of
        Nothing -> Right (stImage, fsImage, Nothing)
        Just levelExpr -> do
          (stA, fsA, levelVal) <- emitExpr stImage fsImage levelExpr
          ensureIndexType (levelVal.valType)
          Right (stA, fsA, Just levelVal)
      let texLayout = texVal.valType
      let isStorage = case texLayout of
            TLStorageTexture1D _ _ -> True
            TLStorageTexture2D _ _ -> True
            TLStorageTexture2DArray _ _ -> True
            TLStorageTexture3D _ _ -> True
            _ -> False
      let isMultisampled = case texLayout of
            TLTextureMultisampled2D _ -> True
            TLTextureDepthMultisampled2D -> True
            _ -> False
      when ((isStorage || isMultisampled) && isJust mLevelVal) $
        Left (CompileError "textureDimensions level is only valid for sampled textures" Nothing Nothing)
      let queryLayout = querySizeLayout texLayout
      let (queryTy, st3) = emitTypeFromLayout st2 queryLayout
      let (resId, st4) = freshId st3
      let st4' = addCapability capabilityImageQuery st4
      (st5, fs5, lodVal) <-
        if isStorage || isMultisampled
          then Right (st4', fs2, Nothing)
          else do
            case mLevelVal of
              Just levelVal -> Right (st4', fs2, Just levelVal)
              Nothing -> do
                let (cid, st') = emitConstU32 st4' 0
                let (a, sz) = scalarLayout U32
                let layout = TLScalar U32 a sz
                Right (st', fs2, Just (Value layout cid))
      let fs6 = case lodVal of
            Nothing -> addFuncInstr (Instr opImageQuerySize [queryTy, resId, texId]) fs5
            Just lod ->
              addFuncInstr (Instr opImageQuerySizeLod [queryTy, resId, texId, lod.valId]) fs5
      let queryVal = Value queryLayout resId
      adjustQueryValue texLayout queryVal st5 fs6

    querySizeLayout texLayout =
      let (a, sz) = scalarLayout U32
          vec n = let (va, vsz) = vectorLayout U32 n in TLVector n U32 va vsz
      in case texLayout of
          TLTexture1D _ -> TLScalar U32 a sz
          TLTexture1DArray _ -> vec 2
          TLTexture2D _ -> vec 2
          TLTexture2DArray _ -> vec 3
          TLTexture3D _ -> vec 3
          TLTextureCube _ -> vec 2
          TLTextureCubeArray _ -> vec 3
          TLTextureMultisampled2D _ -> vec 2
          TLTextureDepth2D -> vec 2
          TLTextureDepth2DArray -> vec 3
          TLTextureDepthCube -> vec 2
          TLTextureDepthCubeArray -> vec 3
          TLTextureDepthMultisampled2D -> vec 2
          TLStorageTexture1D _ _ -> TLScalar U32 a sz
          TLStorageTexture2D _ _ -> vec 2
          TLStorageTexture2DArray _ _ -> vec 3
          TLStorageTexture3D _ _ -> vec 3
          _ -> TLVector 2 U32 a (snd (vectorLayout U32 2))

    adjustQueryValue texLayout queryVal st0 fs0 =
      case texLayout of
        TLTexture1DArray _ -> emitVectorComponent queryVal 0 st0 fs0
        TLTexture2DArray _ -> dropToVec2 queryVal st0 fs0
        TLTextureCubeArray _ -> dropToVec2 queryVal st0 fs0
        TLTextureDepth2DArray -> dropToVec2 queryVal st0 fs0
        TLTextureDepthCubeArray -> dropToVec2 queryVal st0 fs0
        TLStorageTexture2DArray _ _ -> dropToVec2 queryVal st0 fs0
        _ -> Right (st0, fs0, queryVal)

    dropToVec2 vecVal st0 fs0 = do
      (st1, fs1, xVal) <- emitVectorComponent vecVal 0 st0 fs0
      (st2, fs2, yVal) <- emitVectorComponent vecVal 1 st1 fs1
      (st3, fs3, vec2Val) <- emitVec2FromScalarScalar xVal yVal st2 fs2
      Right (st3, fs3, vec2Val)

emitTextureNumLevels :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureNumLevels args st fs =
  case args of
    [texExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      (stImage, fsImage, texId) <- emitImageFromTextureValue st1 fs1 texVal
      case texVal.valType of
        TLTextureMultisampled2D _ -> Left (CompileError "textureNumLevels is not valid for multisampled textures" Nothing Nothing)
        TLTextureDepthMultisampled2D -> Left (CompileError "textureNumLevels is not valid for multisampled textures" Nothing Nothing)
        TLStorageTexture1D _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        TLStorageTexture2D _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        TLStorageTexture2DArray _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        TLStorageTexture3D _ _ -> Left (CompileError "textureNumLevels is not valid for storage textures" Nothing Nothing)
        _ -> do
          let (a, sz) = scalarLayout U32
          let outLayout = TLScalar U32 a sz
          let (outTy, st2) = emitTypeFromLayout stImage outLayout
          let (resId, st3) = freshId st2
          let st3' = addCapability capabilityImageQuery st3
          let fs2 = addFuncInstr (Instr opImageQueryLevels [outTy, resId, texId]) fsImage
          Right (st3', fs2, Value outLayout resId)
    _ -> Left (CompileError "textureNumLevels expects (texture)" Nothing Nothing)

emitTextureNumLayers :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureNumLayers args st fs =
  case args of
    [texExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      (stImage, fsImage, texId) <- emitImageFromTextureValue st1 fs1 texVal
      let texLayout = texVal.valType
      let isArray = case texLayout of
            TLTexture1DArray _ -> True
            TLTexture2DArray _ -> True
            TLTextureCubeArray _ -> True
            TLTextureDepth2DArray -> True
            TLTextureDepthCubeArray -> True
            TLStorageTexture2DArray _ _ -> True
            _ -> False
      unless isArray $
        Left (CompileError "textureNumLayers expects an array texture" Nothing Nothing)
      let queryLayout = case texLayout of
            TLTexture1DArray _ -> let (va, vsz) = vectorLayout U32 2 in TLVector 2 U32 va vsz
            _ -> let (va, vsz) = vectorLayout U32 3 in TLVector 3 U32 va vsz
      let (outTy, st2) = emitTypeFromLayout stImage queryLayout
      let (resId, st3) = freshId st2
      let st3' = addCapability capabilityImageQuery st3
      let isStorage = case texLayout of
            TLStorageTexture2DArray _ _ -> True
            _ -> False
      (st4, fs2) <-
        if isStorage
          then Right (st3', addFuncInstr (Instr opImageQuerySize [outTy, resId, texId]) fsImage)
          else do
            let (cid, st') = emitConstU32 st3' 0
                (a, sz) = scalarLayout U32
                lodVal = Value (TLScalar U32 a sz) cid
            Right (st', addFuncInstr (Instr opImageQuerySizeLod [outTy, resId, texId, lodVal.valId]) fsImage)
      let queryVal = Value queryLayout resId
      let layerIx = case texLayout of
            TLTexture1DArray _ -> 1
            _ -> 2
      emitVectorComponent queryVal layerIx st4 fs2
    _ -> Left (CompileError "textureNumLayers expects (texture)" Nothing Nothing)

emitTextureNumSamples :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureNumSamples args st fs =
  case args of
    [texExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      case texVal.valType of
        TLTextureMultisampled2D _ -> querySamples texVal st1 fs1
        TLTextureDepthMultisampled2D -> querySamples texVal st1 fs1
        _ -> Left (CompileError "textureNumSamples expects a multisampled texture" Nothing Nothing)
    _ -> Left (CompileError "textureNumSamples expects (texture)" Nothing Nothing)
  where
    querySamples texVal st0 fs0 = do
      (stImage, fsImage, texId) <- emitImageFromTextureValue st0 fs0 texVal
      let (a, sz) = scalarLayout U32
      let outLayout = TLScalar U32 a sz
      let (outTy, st1) = emitTypeFromLayout stImage outLayout
      let (resId, st2) = freshId st1
      let st2' = addCapability capabilityImageQuery st2
      let fs1 = addFuncInstr (Instr opImageQuerySamples [outTy, resId, texId]) fsImage
      Right (st2', fs1, Value outLayout resId)

emitTextureLoad :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureLoad args st fs =
  case args of
    [texExpr, coordExpr] ->
      emitStorageLoad texExpr coordExpr st fs
    [texExpr, coordExpr, thirdExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      case texVal.valType of
        TLTextureMultisampled2D _ ->
          emitMultisampledLoad texVal coordExpr thirdExpr st1 fs1
        TLTextureDepthMultisampled2D ->
          emitMultisampledLoad texVal coordExpr thirdExpr st1 fs1
        _ ->
          emitSampledLoad texVal coordExpr Nothing thirdExpr st1 fs1
    [texExpr, coordExpr, arrayExpr, levelExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      emitSampledLoad texVal coordExpr (Just arrayExpr) levelExpr st1 fs1
    _ -> Left (CompileError "textureLoad expects (texture, coords[, array_index], level) or (texture_multisampled_2d, coords, sample_index)" Nothing Nothing)
  where
    emitStorageLoad texExpr coordExpr st0 fs0 = do
      (st1, fs1, texVal) <- emitExpr st0 fs0 texExpr
      (st2, fs2, coordVal) <- emitExpr st1 fs1 coordExpr
      (scalar, access) <- case texVal.valType of
        TLStorageTexture1D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2DArray fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture3D fmt acc -> Right (storageFormatScalar fmt, acc)
        _ -> Left (CompileError "textureLoad expects a storage texture" Nothing Nothing)
      when (access == StorageWrite) $
        Left (CompileError "textureLoad is not allowed on write-only storage textures" Nothing Nothing)
      case texVal.valType of
        TLStorageTexture1D _ _ -> ensureIndexType (coordVal.valType)
        TLStorageTexture2D _ _ -> ensureIntVec2 (coordVal.valType)
        TLStorageTexture2DArray _ _ -> ensureIntVec3 (coordVal.valType)
        TLStorageTexture3D _ _ -> ensureIntVec3 (coordVal.valType)
        _ -> Left (CompileError "invalid storage texture coordinates" Nothing Nothing)
      let (align, size) = vectorLayout scalar 4
      let outLayout = TLVector 4 scalar align size
      let (outTy, st3) = emitTypeFromLayout st2 outLayout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opImageRead [outTy, resId, texVal.valId, coordVal.valId]) fs2
      Right (st4, fs3, Value outLayout resId)

    emitSampledLoad texVal coordExpr mArray levelExpr st0 fs0 = do
      (stImage, fsImage, texId) <- emitImageFromTextureValue st0 fs0 texVal
      (st1, fs1, coordVal) <- emitExpr stImage fsImage coordExpr
      (st2, fs2, levelVal) <- emitExpr st1 fs1 levelExpr
      ensureIndexType (levelVal.valType)
      (st3, fs3, coordVal') <-
        case (texVal.valType, mArray) of
          (TLTexture1D _, Nothing) -> do
            ensureIndexType (coordVal.valType)
            Right (st2, fs2, coordVal)
          (TLTexture1DArray _, Just arrayExpr) -> do
            ensureIndexType (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec2FromScalarScalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTexture2D _, Nothing) -> do
            ensureIntVec2 (coordVal.valType)
            Right (st2, fs2, coordVal)
          (TLTexture3D _, Nothing) -> do
            ensureIntVec3 (coordVal.valType)
            Right (st2, fs2, coordVal)
          (TLTextureCube _, Nothing) -> do
            ensureIntVec3 (coordVal.valType)
            Right (st2, fs2, coordVal)
          (TLTextureDepth2D, Nothing) -> do
            ensureIntVec2 (coordVal.valType)
            Right (st2, fs2, coordVal)
          (TLTextureDepthCube, Nothing) -> do
            ensureIntVec3 (coordVal.valType)
            Right (st2, fs2, coordVal)
          (TLTexture2DArray _, Just arrayExpr) -> do
            ensureIntVec2 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepth2DArray, Just arrayExpr) -> do
            ensureIntVec2 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec3FromVec2Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureCubeArray _, Just arrayExpr) -> do
            ensureIntVec3 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTextureDepthCubeArray, Just arrayExpr) -> do
            ensureIntVec3 (coordVal.valType)
            (stA, fsA, arrayVal) <- emitExpr st2 fs2 arrayExpr
            ensureIndexType (arrayVal.valType)
            (stB, fsB, coordVal') <- emitVec4FromVec3Scalar coordVal arrayVal stA fsA
            Right (stB, fsB, coordVal')
          (TLTexture1DArray _, Nothing) ->
            Left (CompileError "textureLoad for texture_1d_array requires an array index" Nothing Nothing)
          (TLTexture2DArray _, Nothing) ->
            Left (CompileError "textureLoad for texture_2d_array requires an array index" Nothing Nothing)
          (TLTextureCubeArray _, Nothing) ->
            Left (CompileError "textureLoad for texture_cube_array requires an array index" Nothing Nothing)
          (TLTextureDepth2DArray, Nothing) ->
            Left (CompileError "textureLoad for texture_depth_2d_array requires an array index" Nothing Nothing)
          (TLTextureDepthCubeArray, Nothing) ->
            Left (CompileError "textureLoad for texture_depth_cube_array requires an array index" Nothing Nothing)
          _ -> Left (CompileError "textureLoad expects a sampled texture binding" Nothing Nothing)
      let (fetchLayout, resultLayout) =
            case texVal.valType of
              TLTextureDepth2D ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, uncurry (TLScalar F32) (scalarLayout F32))
              TLTextureDepth2DArray ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, uncurry (TLScalar F32) (scalarLayout F32))
              TLTextureDepthCube ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, uncurry (TLScalar F32) (scalarLayout F32))
              TLTextureDepthCubeArray ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, uncurry (TLScalar F32) (scalarLayout F32))
              TLTexture2D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture2DArray s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture3D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTextureCube s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTextureCubeArray s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture1D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              TLTexture1DArray s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              _ -> let (a, sz) = vectorLayout F32 4 in (TLVector 4 F32 a sz, TLVector 4 F32 a sz)
      let (outTy, st4) = emitTypeFromLayout st3 fetchLayout
      let (resId, st5) = freshId st4
      let fs4 = addFuncInstr (Instr opImageFetch [outTy, resId, texId, coordVal'.valId, imageOperandsLod, levelVal.valId]) fs3
      let fetchVal = Value fetchLayout resId
      if fetchLayout == resultLayout
        then Right (st5, fs4, fetchVal)
        else emitVectorComponent fetchVal 0 st5 fs4

    emitMultisampledLoad texVal coordExpr sampleExpr st0 fs0 = do
      (stImage, fsImage, texId) <- emitImageFromTextureValue st0 fs0 texVal
      (st1, fs1, coordVal) <- emitExpr stImage fsImage coordExpr
      (st2, fs2, sampleVal) <- emitExpr st1 fs1 sampleExpr
      ensureIndexType (sampleVal.valType)
      case texVal.valType of
        TLTextureMultisampled2D _ -> ensureIntVec2 (coordVal.valType)
        TLTextureDepthMultisampled2D -> ensureIntVec2 (coordVal.valType)
        _ -> Left (CompileError "textureLoad expects a multisampled texture" Nothing Nothing)
      let (fetchLayout, resultLayout) =
            case texVal.valType of
              TLTextureDepthMultisampled2D ->
                let (a, sz) = vectorLayout F32 4
                in (TLVector 4 F32 a sz, uncurry (TLScalar F32) (scalarLayout F32))
              TLTextureMultisampled2D s -> let (a, sz) = vectorLayout s 4 in (TLVector 4 s a sz, TLVector 4 s a sz)
              _ -> let (a, sz) = vectorLayout F32 4 in (TLVector 4 F32 a sz, TLVector 4 F32 a sz)
      let (outTy, st3) = emitTypeFromLayout st2 fetchLayout
      let (resId, st4) = freshId st3
      let fs3 = addFuncInstr (Instr opImageFetch [outTy, resId, texId, coordVal.valId, imageOperandsSample, sampleVal.valId]) fs2
      let fetchVal = Value fetchLayout resId
      if fetchLayout == resultLayout
        then Right (st4, fs3, fetchVal)
        else emitVectorComponent fetchVal 0 st4 fs3

emitTextureStore :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitTextureStore args st fs =
  case args of
    [texExpr, coordExpr, valueExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      (st2, fs2, coordVal) <- emitExpr st1 fs1 coordExpr
      (st3, fs3, valueVal) <- emitExpr st2 fs2 valueExpr
      (scalar, access) <- case texVal.valType of
        TLStorageTexture1D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2D fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture2DArray fmt acc -> Right (storageFormatScalar fmt, acc)
        TLStorageTexture3D fmt acc -> Right (storageFormatScalar fmt, acc)
        _ -> Left (CompileError "textureStore expects a storage texture" Nothing Nothing)
      when (access == StorageRead) $
        Left (CompileError "textureStore is not allowed on read-only storage textures" Nothing Nothing)
      case texVal.valType of
        TLStorageTexture1D _ _ -> ensureIndexType (coordVal.valType)
        TLStorageTexture2D _ _ -> ensureIntVec2 (coordVal.valType)
        TLStorageTexture2DArray _ _ -> ensureIntVec3 (coordVal.valType)
        TLStorageTexture3D _ _ -> ensureIntVec3 (coordVal.valType)
        _ -> Left (CompileError "textureStore expects a storage texture" Nothing Nothing)
      let (align, size) = vectorLayout scalar 4
      let expectedLayout = TLVector 4 scalar align size
      ensureTypeMatch expectedLayout (valueVal.valType)
      let fs4 = addFuncInstr (Instr opImageWrite [texVal.valId, coordVal.valId, valueVal.valId]) fs3
      Right (st3, fs4)
    _ -> Left (CompileError "textureStore expects (texture, coords, value)" Nothing Nothing)

emitDerivative :: Word16 -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitDerivative opcode args st fs =
  case args of
    [arg] -> do
      when (st.gsEntryStage /= StageFragment) $
        Left (CompileError "derivatives are only available in fragment shaders" Nothing Nothing)
      (st1, fs1, val) <- emitExpr st fs arg
      ensureFloatNumeric (val.valType)
      let (tyId, st2) = emitTypeFromLayout st1 (val.valType)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opcode [tyId, resId, val.valId]) fs1
      Right (st3, fs2, Value (val.valType) resId)
    _ -> Left (CompileError "derivative builtins expect one argument" Nothing Nothing)

emitAtomicPtr :: Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState, VarInfo)
emitAtomicPtr expr st fs =
  case exprToLValue expr of
    Nothing -> Left (CompileError "atomic operations require an addressable expression" Nothing Nothing)
    Just lv -> do
      (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
      case ptrInfo.viType of
        TLAtomic _ -> pure ()
        _ -> Left (CompileError "atomic operations require atomic types" Nothing Nothing)
      when (ptrInfo.viStorage /= storageClassStorageBuffer) $
        Left (CompileError "atomic operations are only supported on storage buffers" Nothing Nothing)
      when (ptrInfo.viAccess == ReadOnly) $
        Left (CompileError "atomic operations require read_write storage buffers" Nothing Nothing)
      Right (st1, fs1, ptrInfo)

emitAtomicLoad :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAtomicLoad args st fs =
  case args of
    [ptrExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case ptrInfo.viType of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomicLoad expects an atomic value" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let layout = TLScalar scalar a sz
      let (tyId, st2) = emitTypeFromLayout st1 layout
      let (resId, st3) = freshId st2
      (st4, fs2, ptrId) <- resolveVarPtr st3 fs1 ptrInfo
      let (scopeId, st5) = emitConstU32 st4 memoryScopeDevice
      let (semId, st6) = emitConstU32 st5 memorySemanticsRelaxed
      let fs3 = addFuncInstr (Instr opAtomicLoad [tyId, resId, ptrId, scopeId, semId]) fs2
      Right (st6, fs3, Value layout resId)
    _ -> Left (CompileError "atomicLoad expects (ptr)" Nothing Nothing)

emitAtomicStore :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitAtomicStore args st fs =
  case args of
    [ptrExpr, valueExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case ptrInfo.viType of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomicStore expects an atomic value" Nothing Nothing)
      (st2, fs2, val) <- emitExpr st1 fs1 valueExpr
      let (a, sz) = scalarLayout scalar
      (st3, fs3, val') <- coerceValueToLayout (TLScalar scalar a sz) val st2 fs2
      (st4, fs4, ptrId) <- resolveVarPtr st3 fs3 ptrInfo
      let (scopeId, st5) = emitConstU32 st4 memoryScopeDevice
      let (semId, st6) = emitConstU32 st5 memorySemanticsRelaxed
      let fs5 = addFuncInstr (Instr opAtomicStore [ptrId, scopeId, semId, val'.valId]) fs4
      Right (st6, fs5)
    _ -> Left (CompileError "atomicStore expects (ptr, value)" Nothing Nothing)

emitAtomicBinary :: (Scalar -> Either CompileError Word16) -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAtomicBinary opcodeFor args st fs =
  case args of
    [ptrExpr, valueExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case ptrInfo.viType of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomic operation expects an atomic value" Nothing Nothing)
      opcode <- opcodeFor scalar
      (st2, fs2, val) <- emitExpr st1 fs1 valueExpr
      let (a, sz) = scalarLayout scalar
      (st3, fs3, val') <- coerceValueToLayout (TLScalar scalar a sz) val st2 fs2
      let layout = TLScalar scalar a sz
      let (tyId, st4) = emitTypeFromLayout st3 layout
      let (resId, st5) = freshId st4
      (st6, fs4, ptrId) <- resolveVarPtr st5 fs3 ptrInfo
      let (scopeId, st7) = emitConstU32 st6 memoryScopeDevice
      let (semId, st8) = emitConstU32 st7 memorySemanticsRelaxed
      let fs5 = addFuncInstr (Instr opcode [tyId, resId, ptrId, scopeId, semId, val'.valId]) fs4
      Right (st8, fs5, Value layout resId)
    _ -> Left (CompileError "atomic operation expects (ptr, value)" Nothing Nothing)

emitAtomicCompareExchangeWeak :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitAtomicCompareExchangeWeak args st fs =
  case args of
    [ptrExpr, cmpExpr, valueExpr] -> do
      (st1, fs1, ptrInfo) <- emitAtomicPtr ptrExpr st fs
      scalar <- case ptrInfo.viType of
        TLAtomic s -> Right s
        _ -> Left (CompileError "atomicCompareExchangeWeak expects an atomic value" Nothing Nothing)
      let (a, sz) = scalarLayout scalar
      let scalarTl = TLScalar scalar a sz
      (st2, fs2, cmpVal) <- emitExpr st1 fs1 cmpExpr
      (st3, fs3, cmpVal') <- coerceValueToLayout scalarTl cmpVal st2 fs2
      (st4, fs4, valueVal) <- emitExpr st3 fs3 valueExpr
      (st5, fs5, valueVal') <- coerceValueToLayout scalarTl valueVal st4 fs4
      (st6, fs6, ptrId) <- resolveVarPtr st5 fs5 ptrInfo
      let (oldTyId, st7) = emitTypeFromLayout st6 scalarTl
      let (oldId, st8) = freshId st7
      let (scopeId, st9) = emitConstU32 st8 memoryScopeDevice
      let (eqSemId, st10) = emitConstU32 st9 memorySemanticsRelaxed
      let (neqSemId, st11) = emitConstU32 st10 memorySemanticsRelaxed
      let fs7 =
            addFuncInstr
              (Instr opAtomicCompareExchange [oldTyId, oldId, ptrId, scopeId, eqSemId, neqSemId, valueVal'.valId, cmpVal'.valId])
              fs6
      let (boolA, boolSz) = scalarLayout Bool
      let boolTl = TLScalar Bool boolA boolSz
      let (boolTyId, st12) = emitTypeFromLayout st11 boolTl
      let (eqId, st13) = freshId st12
      let fs8 = addFuncInstr (Instr opIEqual [boolTyId, eqId, oldId, cmpVal'.valId]) fs7
      let resultLayout = atomicCompareExchangeResultLayout scalar
      let (resultTyId, st14) = emitTypeFromLayout st13 resultLayout
      let (resultId, st15) = freshId st14
      let fs9 = addFuncInstr (Instr opCompositeConstruct [resultTyId, resultId, oldId, eqId]) fs8
      Right (st15, fs9, Value resultLayout resultId)
    _ -> Left (CompileError "atomicCompareExchangeWeak expects (ptr, cmp, value)" Nothing Nothing)

atomicCompareExchangeResultLayout :: Scalar -> TypeLayout
atomicCompareExchangeResultLayout scalar =
  let (a, sz) = scalarLayout scalar
      oldLayout = TLScalar scalar a sz
      (ba, bsz) = scalarLayout Bool
      boolLayout = TLScalar Bool ba bsz
      oldField = FieldLayout "old_value" 0 oldLayout a sz
      exchangedOffset = roundUp sz ba
      exchangedField = FieldLayout "exchanged" exchangedOffset boolLayout ba bsz
      align = max a ba
      size = roundUp (exchangedOffset + bsz) align
      structName =
        case scalar of
          I32 -> "__atomic_compare_exchange_result_i32"
          U32 -> "__atomic_compare_exchange_result_u32"
          _ -> "__atomic_compare_exchange_result_i32"
  in TLStruct structName [oldField, exchangedField] align size

emitExprList :: GenState -> FuncState -> [Expr] -> Either CompileError (GenState, FuncState, [Value])
emitExprList st fs [] = Right (st, fs, [])
emitExprList st fs (x:xs) = do
  (st1, fs1, v) <- emitExpr st fs x
  (st2, fs2, vs) <- emitExprList st1 fs1 xs
  Right (st2, fs2, v:vs)

emitLValuePtr :: GenState -> FuncState -> LValue -> Either CompileError (GenState, FuncState, VarInfo)
emitLValuePtr st fs lv =
  case lv of
    LVVar _ name ->
      case Map.lookup name fs.fsVarsByName of
        Just v -> Right (st, fs, v)
        Nothing ->
          case Map.lookup name fs.fsValuesByName of
            Just _ -> Left (CompileError "cannot take the address of an immutable let binding" Nothing Nothing)
            Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
    LVField _ base field -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      case baseInfo.viType of
        TLStruct _ fields _ _ -> do
          (ix, fieldLayout) <- findField (textToString field) fields
          let (ixId, st2) = emitConstU32 st1 (fromIntegral ix)
          emitAccessChain st2 fs1 baseInfo [ixId] fieldLayout
        TLVector n scalar _ _ -> do
          if T.length field /= 1
            then Left (CompileError "cannot assign to vector swizzle" Nothing Nothing)
            else do
              ix <- vectorFieldIndex field n
              let (ixId, st2) = emitConstU32 st1 (fromIntegral ix)
              let (a, sz) = scalarLayout scalar
              let fieldLayout = TLScalar scalar a sz
              emitAccessChain st2 fs1 baseInfo [ixId] fieldLayout
        _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)
    LVIndex _ base idxExpr -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      (st2, fs2, idxVal) <- emitExpr st1 fs1 idxExpr
      ensureIndexType (idxVal.valType)
      case baseInfo.viType of
        TLArray _ _ elemLayout _ _ -> emitAccessChain st2 fs2 baseInfo [idxVal.valId] elemLayout
        TLVector _ scalar _ _ ->
          let (a, sz) = scalarLayout scalar
              elemLayout = TLScalar scalar a sz
          in emitAccessChain st2 fs2 baseInfo [idxVal.valId] elemLayout
        TLMatrix _ rows scalar _ _ _ ->
          let elemLayout = uncurry (TLVector rows scalar) (vectorLayout scalar rows)
          in emitAccessChain st2 fs2 baseInfo [idxVal.valId] elemLayout
        _ -> Left (CompileError "indexing requires array or vector type" Nothing Nothing)
    LVDeref _ expr -> do
      (st1, fs1, val) <- emitExpr st fs expr
      case val.valType of
        TLPointer storageClass access elemLayout ->
          Right (st1, fs1, VarInfo elemLayout (val.valId) storageClass (ptrAccessToVarAccess access) [])
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

emitAccessChain :: GenState -> FuncState -> VarInfo -> [Word32] -> TypeLayout -> Either CompileError (GenState, FuncState, VarInfo)
emitAccessChain st fs baseInfo indices elemLayout = do
  let storageClass = baseInfo.viStorage
  let (elemTy, st1) = emitTypeFromLayout st elemLayout
  let (ptrTy, st2) = emitPointerType st1 storageClass elemTy
  let (resId, st3) = freshId st2
  let instr = Instr opAccessChain (ptrTy : resId : baseInfo.viPtrId : indices)
  let fs1 = addFuncInstr instr fs
  Right (st3, fs1, VarInfo elemLayout resId storageClass (baseInfo.viAccess) [])

ensureTypeMatch :: TypeLayout -> TypeLayout -> Either CompileError ()
ensureTypeMatch a b =
  case (a, b) of
    (TLPointer scA accA elemA, TLPointer scB accB elemB) ->
      if scA == scB && elemA == elemB && ptrAccessCompatible accA accB
        then Right ()
        else Left (CompileError "type mismatch" Nothing Nothing)
    _ ->
      if a == b
        then Right ()
        else Left (CompileError "type mismatch" Nothing Nothing)

ensureWritable :: VarInfo -> Either CompileError ()
ensureWritable info =
  case info.viAccess of
    ReadWrite -> Right ()
    ReadOnly -> Left (CompileError "assignment target is read-only" Nothing Nothing)

ensureBoolScalar :: TypeLayout -> Either CompileError ()
ensureBoolScalar layout =
  case layout of
    TLScalar Bool _ _ -> Right ()
    _ -> Left (CompileError "expected bool scalar" Nothing Nothing)

ensureBoolVectorSize :: Int -> TypeLayout -> Either CompileError ()
ensureBoolVectorSize n layout =
  case layout of
    TLVector m Bool _ _ | m == n -> Right ()
    _ -> Left (CompileError "expected bool vector with matching size" Nothing Nothing)

ensureSwitchType :: TypeLayout -> Either CompileError ()
ensureSwitchType layout =
  case layout of
    TLScalar I32 _ _ -> Right ()
    TLScalar U32 _ _ -> Right ()
    _ -> Left (CompileError "switch selector must be an i32 or u32 scalar" Nothing Nothing)

ensureIntNumeric :: TypeLayout -> Either CompileError ()
ensureIntNumeric layout =
  case layout of
    TLScalar I32 _ _ -> Right ()
    TLScalar U32 _ _ -> Right ()
    TLVector _ I32 _ _ -> Right ()
    TLVector _ U32 _ _ -> Right ()
    _ -> Left (CompileError "expected i32 or u32 scalar or vector" Nothing Nothing)

ensureFloatNumeric :: TypeLayout -> Either CompileError ()
ensureFloatNumeric layout =
  case layout of
    TLScalar F32 _ _ -> Right ()
    TLScalar F16 _ _ -> Right ()
    TLVector _ F32 _ _ -> Right ()
    TLVector _ F16 _ _ -> Right ()
    _ -> Left (CompileError "expected f32 or f16 scalar or vector" Nothing Nothing)

ensureFloatVector :: TypeLayout -> Either CompileError ()
ensureFloatVector layout =
  case layout of
    TLVector _ F32 _ _ -> Right ()
    TLVector _ F16 _ _ -> Right ()
    _ -> Left (CompileError "expected f32 or f16 vector" Nothing Nothing)

ensureIndexType :: TypeLayout -> Either CompileError ()
ensureIndexType layout =
  case layout of
    TLScalar U32 _ _ -> Right ()
    TLScalar I32 _ _ -> Right ()
    _ -> Left (CompileError "index must be u32 or i32" Nothing Nothing)

ensureIntVec2 :: TypeLayout -> Either CompileError ()
ensureIntVec2 layout =
  case layout of
    TLVector 2 U32 _ _ -> Right ()
    TLVector 2 I32 _ _ -> Right ()
    _ -> Left (CompileError "expected vec2<i32> or vec2<u32>" Nothing Nothing)

ensureFloatVec2 :: TypeLayout -> Either CompileError ()
ensureFloatVec2 layout =
  case layout of
    TLVector 2 F32 _ _ -> Right ()
    TLVector 2 F16 _ _ -> Right ()
    _ -> Left (CompileError "expected vec2<f32> or vec2<f16>" Nothing Nothing)

ensureFloatVec3 :: TypeLayout -> Either CompileError ()
ensureFloatVec3 layout =
  case layout of
    TLVector 3 F32 _ _ -> Right ()
    TLVector 3 F16 _ _ -> Right ()
    _ -> Left (CompileError "expected vec3<f32> or vec3<f16>" Nothing Nothing)

ensureFloatVecN :: Int -> TypeLayout -> Either CompileError ()
ensureFloatVecN n layout =
  case layout of
    TLVector m F32 _ _ | m == n -> Right ()
    TLVector m F16 _ _ | m == n -> Right ()
    _ -> Left (CompileError ("expected vec" <> show n <> "<f32> or vec" <> show n <> "<f16>") Nothing Nothing)

ensureIntVec3 :: TypeLayout -> Either CompileError ()
ensureIntVec3 layout =
  case layout of
    TLVector 3 U32 _ _ -> Right ()
    TLVector 3 I32 _ _ -> Right ()
    _ -> Left (CompileError "expected vec3<i32> or vec3<u32>" Nothing Nothing)

ensureFloatMatrix :: TypeLayout -> Either CompileError (Int, Int, Scalar)
ensureFloatMatrix layout =
  case layout of
    TLMatrix cols rows scalar _ _ _ ->
      case scalar of
        F32 -> Right (cols, rows, scalar)
        F16 -> Right (cols, rows, scalar)
        _ -> Left (CompileError "expected float matrix" Nothing Nothing)
    _ -> Left (CompileError "expected matrix type" Nothing Nothing)

ensureFloatScalar :: TypeLayout -> Either CompileError ()
ensureFloatScalar layout =
  case layout of
    TLScalar F32 _ _ -> Right ()
    TLScalar F16 _ _ -> Right ()
    _ -> Left (CompileError "expected f32 or f16 scalar" Nothing Nothing)

findField :: String -> [FieldLayout] -> Either CompileError (Int, TypeLayout)
findField name = go 0
  where
    go _ [] = Left (CompileError ("unknown field: " <> name) Nothing Nothing)
    go ix (f:fs)
      | f.flName == name = Right (ix, f.flType)
      | otherwise = go (ix + 1) fs

classifyNumeric :: TypeLayout -> Maybe (Int, Scalar)
classifyNumeric layout =
  case layout of
    TLScalar s _ _ -> Just (1, s)
    TLVector n s _ _ -> Just (n, s)
    TLAtomic _ -> Nothing
    _ -> Nothing

boolResultLayout :: Int -> TypeLayout
boolResultLayout n =
  if n <= 1
    then
      let (a, sz) = scalarLayout Bool
      in TLScalar Bool a sz
    else
      let (a, sz) = vectorLayout Bool n
      in TLVector n Bool a sz

buildSpirvBytes :: CompileOptions -> EntryPoint -> GenState -> ByteString
buildSpirvBytes opts entry st =
  spirvToBytes (header <> body)
  where
    header =
      [0x07230203, opts.spirvVersion, 0, st.gsNextId, 0]

    entryPointInstr =
      case st.gsEntryPoint of
        Nothing -> mempty
        Just epId ->
          let nameWords = encodeString (textToString entry.epName)
              model = case entry.epStage of
                StageCompute -> executionModelGLCompute
                StageFragment -> executionModelFragment
                StageVertex -> executionModelVertex
              operands = model : epId : nameWords <> st.gsInterfaceIds
          in encodeInstr (Instr opEntryPoint operands)

    execModeInstr =
      case st.gsEntryPoint of
        Nothing -> mempty
        Just epId ->
          case entry.epStage of
            StageCompute ->
              case entry.epWorkgroupSize of
                Nothing -> mempty
                Just (WorkgroupSizeValue (x, y, z)) ->
                  let ops = [epId, executionModeLocalSize, x, y, z]
                  in encodeInstr (Instr opExecutionMode ops)
                Just (WorkgroupSizeExpr _) -> mempty
            StageFragment ->
              encodeInstr (Instr opExecutionMode [epId, executionModeOriginUpperLeft])
            StageVertex -> mempty

    capInstrs =
      concatMap (\cap -> encodeInstr (Instr opCapability [cap])) (capabilityShader : st.gsCapabilities)

    body =
      mconcat
        [ capInstrs
        , concatMap encodeInstr (reverse st.gsExtInstImports)
        , encodeInstr (Instr opMemoryModel [addressingLogical, memoryModelGLSL450])
        , entryPointInstr
        , execModeInstr
        , concatMap encodeInstr (reverse st.gsNames)
        , concatMap encodeInstr (reverse st.gsDecorations)
        , concatMap encodeInstr (reverse st.gsTypes)
        , concatMap encodeInstr (reverse st.gsConstants)
        , concatMap encodeInstr (reverse st.gsGlobals)
        , concatMap encodeInstr (reverse st.gsFunctions)
        ]

emitVoidType :: GenState -> (Word32, GenState)
emitVoidType st = emitTypeCached st TKVoid (Instr opTypeVoid [])

emitBoolType :: GenState -> (Word32, GenState)
emitBoolType st = emitTypeCached st TKBool (Instr opTypeBool [])

emitIntType :: Bool -> GenState -> (Word32, GenState)
emitIntType signed st =
  let key = TKInt signed 32
      instr = Instr opTypeInt [32, if signed then 1 else 0]
  in emitTypeCached st key instr

emitFloatTypeWidth :: Word32 -> GenState -> (Word32, GenState)
emitFloatTypeWidth bits st =
  let st1 =
        if bits == 16
          then addCapability capabilityFloat16 st
          else st
  in emitTypeCached st1 (TKFloat bits) (Instr opTypeFloat [bits])

emitFloatType :: GenState -> (Word32, GenState)
emitFloatType = emitFloatTypeWidth 32

emitVecType :: Word32 -> Word32 -> GenState -> (Word32, GenState)
emitVecType elemId n st =
  emitTypeCached st (TKVector elemId n) (Instr opTypeVector [elemId, n])

emitSamplerType :: GenState -> (Word32, GenState)
emitSamplerType st =
  emitTypeCached st TKSampler (Instr opTypeSampler [])

emitImageType :: Scalar -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> TypeKey -> GenState -> (Word32, GenState)
emitImageType scalar dim depth arrayed ms sampled fmt key st =
  let (a, sz) = scalarLayout scalar
      (sampledId, st1) = emitTypeFromLayout st (TLScalar scalar a sz)
      instr = Instr opTypeImage [sampledId, dim, depth, arrayed, ms, sampled, fmt]
  in emitTypeCached st1 key instr

emitImage1DType :: Scalar -> GenState -> (Word32, GenState)
emitImage1DType scalar st =
  emitImageType scalar dim1D 0 0 0 imageSampled imageFormatUnknown (TKImage1D scalar) (addCapability capabilitySampled1D st)

emitImage1DArrayType :: Scalar -> GenState -> (Word32, GenState)
emitImage1DArrayType scalar st =
  emitImageType scalar dim1D 0 1 0 imageSampled imageFormatUnknown (TKImage1DArray scalar) (addCapability capabilitySampled1D st)

emitImage2DType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DType scalar =
  emitImageType scalar dim2D 0 0 0 imageSampled imageFormatUnknown (TKImage2D scalar)

emitImage2DArrayType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DArrayType scalar =
  emitImageType scalar dim2D 0 1 0 imageSampled imageFormatUnknown (TKImage2DArray scalar)

emitImage3DType :: Scalar -> GenState -> (Word32, GenState)
emitImage3DType scalar =
  emitImageType scalar dim3D 0 0 0 imageSampled imageFormatUnknown (TKImage3D scalar)

emitImageCubeType :: Scalar -> GenState -> (Word32, GenState)
emitImageCubeType scalar =
  emitImageType scalar dimCube 0 0 0 imageSampled imageFormatUnknown (TKImageCube scalar)

emitImageCubeArrayType :: Scalar -> GenState -> (Word32, GenState)
emitImageCubeArrayType scalar =
  emitImageType scalar dimCube 0 1 0 imageSampled imageFormatUnknown (TKImageCubeArray scalar)

emitImage2DMultisampledType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DMultisampledType scalar =
  emitImageType scalar dim2D 0 0 1 imageSampled imageFormatUnknown (TKImage2DMultisampled scalar)

emitImageDepth2DType :: GenState -> (Word32, GenState)
emitImageDepth2DType =
  emitImageType F32 dim2D 1 0 0 imageSampled imageFormatUnknown TKImageDepth2D

emitImageDepth2DArrayType :: GenState -> (Word32, GenState)
emitImageDepth2DArrayType =
  emitImageType F32 dim2D 1 1 0 imageSampled imageFormatUnknown TKImageDepth2DArray

emitImageDepthCubeType :: GenState -> (Word32, GenState)
emitImageDepthCubeType =
  emitImageType F32 dimCube 1 0 0 imageSampled imageFormatUnknown TKImageDepthCube

emitImageDepthCubeArrayType :: GenState -> (Word32, GenState)
emitImageDepthCubeArrayType =
  emitImageType F32 dimCube 1 1 0 imageSampled imageFormatUnknown TKImageDepthCubeArray

emitImageDepth2DMultisampledType :: GenState -> (Word32, GenState)
emitImageDepth2DMultisampledType =
  emitImageType F32 dim2D 1 0 1 imageSampled imageFormatUnknown TKImageDepth2DMultisampled

emitStorageImage1DType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage1DType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim1D 0 0 0 imageStorage fmtId (TKStorageImage1D fmt) (addCapability capabilityImage1D st)

emitStorageImage2DType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage2DType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim2D 0 0 0 imageStorage fmtId (TKStorageImage2D fmt) st

emitStorageImage2DArrayType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage2DArrayType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim2D 0 1 0 imageStorage fmtId (TKStorageImage2DArray fmt) st

emitStorageImage3DType :: StorageFormat -> GenState -> (Word32, GenState)
emitStorageImage3DType fmt st =
  let scalar = storageFormatScalar fmt
      fmtId = storageFormatToImageFormat fmt
  in emitImageType scalar dim3D 0 0 0 imageStorage fmtId (TKStorageImage3D fmt) st

emitSampledImageType :: Word32 -> GenState -> (Word32, GenState)
emitSampledImageType imageId st =
  emitTypeCached st (TKSampledImage imageId) (Instr opTypeSampledImage [imageId])

emitArrayType :: Word32 -> Word32 -> Word32 -> GenState -> (Word32, GenState)
emitArrayType elemId lenVal stride st =
  let key = TKArray elemId (Just lenVal) stride
  in case Map.lookup key (st.gsTypeCache) of
       Just tid -> (tid, st)
       Nothing ->
         let (lenId, st1) = emitTypeConstU32 st lenVal
             instr = Instr opTypeArray [elemId, lenId]
             (tyId, st2) = emitTypeCached st1 key instr
             st3 = addDecoration (Instr opDecorate [tyId, decorationArrayStride, stride]) st2
         in (tyId, st3)

emitRuntimeArrayType :: Word32 -> Word32 -> GenState -> (Word32, GenState)
emitRuntimeArrayType elemId stride st =
  let key = TKArray elemId Nothing stride
      instr = Instr opTypeRuntimeArray [elemId]
  in case Map.lookup key (st.gsTypeCache) of
       Just tid -> (tid, st)
       Nothing ->
         let (tyId, st1) = emitTypeCached st key instr
             st2 = addDecoration (Instr opDecorate [tyId, decorationArrayStride, stride]) st1
         in (tyId, st2)

emitPointerType :: GenState -> Word32 -> Word32 -> (Word32, GenState)
emitPointerType st storageClass baseId =
  emitTypeCached st (TKPointer storageClass baseId) (Instr opTypePointer [storageClass, baseId])

emitFunctionType :: GenState -> Word32 -> [Word32] -> (Word32, GenState)
emitFunctionType st retTy paramTys =
  emitTypeCached st (TKFunction retTy paramTys) (Instr opTypeFunction (retTy : paramTys))

emitTypeFromLayout :: GenState -> TypeLayout -> (Word32, GenState)
emitTypeFromLayout st layout =
  case layout of
    TLScalar s _ _ ->
      case s of
        Bool -> emitBoolType st
        I32 -> emitIntType True st
        U32 -> emitIntType False st
        F32 -> emitFloatType st
        F16 -> emitFloatTypeWidth 16 st
    TLVector n s _ _ ->
      let (a, sz) = scalarLayout s
          (elemId, st1) = emitTypeFromLayout st (TLScalar s a sz)
      in emitVecType elemId (fromIntegral n) st1
    TLMatrix cols rows s _ _ _ ->
      let (a, sz) = scalarLayout s
          (elemId, st1) = emitTypeFromLayout st (TLScalar s a sz)
          (vecId, st2) = emitVecType elemId (fromIntegral rows) st1
          key = TKMatrix (fromIntegral cols) (fromIntegral rows) s
          instr = Instr opTypeMatrix [vecId, fromIntegral cols]
      in emitTypeCached st2 key instr
    TLSampler -> emitSamplerType st
    TLSamplerComparison -> emitSamplerType st
    TLTexture1D s ->
      let (imageId, st1) = emitImage1DType s st
      in emitSampledImageIfNeeded imageId st1
    TLTexture1DArray s ->
      let (imageId, st1) = emitImage1DArrayType s st
      in emitSampledImageIfNeeded imageId st1
    TLTexture2D s ->
      let (imageId, st1) = emitImage2DType s st
      in emitSampledImageIfNeeded imageId st1
    TLTexture2DArray s ->
      let (imageId, st1) = emitImage2DArrayType s st
      in emitSampledImageIfNeeded imageId st1
    TLTexture3D s ->
      let (imageId, st1) = emitImage3DType s st
      in emitSampledImageIfNeeded imageId st1
    TLTextureCube s ->
      let (imageId, st1) = emitImageCubeType s st
      in emitSampledImageIfNeeded imageId st1
    TLTextureCubeArray s ->
      let (imageId, st1) = emitImageCubeArrayType s st
      in emitSampledImageIfNeeded imageId st1
    TLTextureMultisampled2D s ->
      let (imageId, st1) = emitImage2DMultisampledType s st
      in emitSampledImageIfNeeded imageId st1
    TLTextureDepth2D ->
      let (imageId, st1) = emitImageDepth2DType st
      in emitSampledImageIfNeeded imageId st1
    TLTextureDepth2DArray ->
      let (imageId, st1) = emitImageDepth2DArrayType st
      in emitSampledImageIfNeeded imageId st1
    TLTextureDepthCube ->
      let (imageId, st1) = emitImageDepthCubeType st
      in emitSampledImageIfNeeded imageId st1
    TLTextureDepthCubeArray ->
      let (imageId, st1) = emitImageDepthCubeArrayType st
      in emitSampledImageIfNeeded imageId st1
    TLTextureDepthMultisampled2D ->
      let (imageId, st1) = emitImageDepth2DMultisampledType st
      in emitSampledImageIfNeeded imageId st1
    TLStorageTexture1D fmt _ -> emitStorageImage1DType fmt st
    TLStorageTexture2D fmt _ -> emitStorageImage2DType fmt st
    TLStorageTexture2DArray fmt _ -> emitStorageImage2DArrayType fmt st
    TLStorageTexture3D fmt _ -> emitStorageImage3DType fmt st
    TLAtomic s ->
      case s of
        I32 -> emitIntType True st
        U32 -> emitIntType False st
        _ -> emitIntType True st
    TLPointer storageClass _ elemLayout ->
      let (elemId, st1) = emitTypeFromLayout st elemLayout
      in emitPointerType st1 storageClass elemId
    TLArray mlen stride elemLayout _ _ ->
      let (elemId, st1) = emitTypeFromLayout st elemLayout
      in case mlen of
        Nothing -> emitRuntimeArrayType elemId stride st1
        Just n ->
          emitArrayType elemId (fromIntegral n) stride st1
    TLStruct name fields _ _ ->
      let nameT = T.pack name
      in case lookup nameT (st.gsStructIds) of
        Just sid -> (sid, st)
        Nothing ->
          let (sid, st1) = freshId st
              st2 = st1 { gsStructIds = (nameT, sid) : st1.gsStructIds }
              (st3, fieldTypeIds) = mapAccumL emitFieldType st2 fields
              st4 = addType (Instr opTypeStruct (sid : fieldTypeIds)) st3
              st5 = addName (Instr opName (sid : encodeString name)) st4
              st6 = foldl' (emitMemberDecorate sid) st5 (zip [0 :: Int ..] fields)
              st7 = if nameT `elem` st6.gsBlockStructs
                then addDecoration (Instr opDecorate [sid, decorationBlock]) st6
                else st6
              st8 = foldl' (emitMemberName sid) st7 (zip [0 :: Int ..] fields)
          in (sid, st8)
  where
    emitFieldType st' field =
      let (tyId, st'') = emitTypeFromLayout st' field.flType
      in (st'', tyId)
    emitSampledImageIfNeeded imageId st' =
      if st'.gsSamplerMode == SamplerCombined
        then emitSampledImageType imageId st'
        else (imageId, st')
    emitMemberDecorate structId st' (ix, field) =
      let offset = field.flOffset
      in addDecoration (Instr opMemberDecorate [structId, fromIntegral ix, decorationOffset, offset]) st'
    emitMemberName structId st' (ix, field) =
      addName (Instr opMemberName (structId : fromIntegral ix : encodeString field.flName)) st'

isSampledTextureLayout :: TypeLayout -> Bool
isSampledTextureLayout layout =
  case layout of
    TLTexture1D _ -> True
    TLTexture1DArray _ -> True
    TLTexture2D _ -> True
    TLTexture2DArray _ -> True
    TLTexture3D _ -> True
    TLTextureCube _ -> True
    TLTextureCubeArray _ -> True
    TLTextureMultisampled2D _ -> True
    TLTextureDepth2D -> True
    TLTextureDepth2DArray -> True
    TLTextureDepthCube -> True
    TLTextureDepthCubeArray -> True
    TLTextureDepthMultisampled2D -> True
    _ -> False

emitImageTypeFromTextureLayout :: GenState -> TypeLayout -> Either CompileError (Word32, GenState)
emitImageTypeFromTextureLayout st layout =
  case layout of
    TLTexture1D s -> Right (emitImage1DType s st)
    TLTexture1DArray s -> Right (emitImage1DArrayType s st)
    TLTexture2D s -> Right (emitImage2DType s st)
    TLTexture2DArray s -> Right (emitImage2DArrayType s st)
    TLTexture3D s -> Right (emitImage3DType s st)
    TLTextureCube s -> Right (emitImageCubeType s st)
    TLTextureCubeArray s -> Right (emitImageCubeArrayType s st)
    TLTextureMultisampled2D s -> Right (emitImage2DMultisampledType s st)
    TLTextureDepth2D -> Right (emitImageDepth2DType st)
    TLTextureDepth2DArray -> Right (emitImageDepth2DArrayType st)
    TLTextureDepthCube -> Right (emitImageDepthCubeType st)
    TLTextureDepthCubeArray -> Right (emitImageDepthCubeArrayType st)
    TLTextureDepthMultisampled2D -> Right (emitImageDepth2DMultisampledType st)
    TLStorageTexture1D fmt _ -> Right (emitStorageImage1DType fmt st)
    TLStorageTexture2D fmt _ -> Right (emitStorageImage2DType fmt st)
    TLStorageTexture2DArray fmt _ -> Right (emitStorageImage2DArrayType fmt st)
    TLStorageTexture3D fmt _ -> Right (emitStorageImage3DType fmt st)
    _ -> Left (CompileError "expected a texture type" Nothing Nothing)

emitImageFromTextureValue :: GenState -> FuncState -> Value -> Either CompileError (GenState, FuncState, Word32)
emitImageFromTextureValue st fs texVal
  | st.gsSamplerMode == SamplerCombined && isSampledTextureLayout (texVal.valType) = do
      (imageTy, st1) <- emitImageTypeFromTextureLayout st (texVal.valType)
      let (resId, st2) = freshId st1
      let fs1 = addFuncInstr (Instr opImage [imageTy, resId, texVal.valId]) fs
      Right (st2, fs1, resId)
  | otherwise =
      Right (st, fs, texVal.valId)

emitPointerForBinding :: GenState -> BindingKind -> TypeLayout -> (Word32, GenState)
emitPointerForBinding st kind layout =
  let storageClass = case kind of
        BUniform -> storageClassUniform
        BStorageRead -> storageClassStorageBuffer
        BStorageReadWrite -> storageClassStorageBuffer
        BSampler -> storageClassUniformConstant
        BSamplerComparison -> storageClassUniformConstant
        BTexture1D -> storageClassUniformConstant
        BTexture1DArray -> storageClassUniformConstant
        BTexture2D -> storageClassUniformConstant
        BTexture2DArray -> storageClassUniformConstant
        BTexture3D -> storageClassUniformConstant
        BTextureCube -> storageClassUniformConstant
        BTextureCubeArray -> storageClassUniformConstant
        BTextureMultisampled2D -> storageClassUniformConstant
        BTextureDepth2D -> storageClassUniformConstant
        BTextureDepth2DArray -> storageClassUniformConstant
        BTextureDepthCube -> storageClassUniformConstant
        BTextureDepthCubeArray -> storageClassUniformConstant
        BTextureDepthMultisampled2D -> storageClassUniformConstant
        BStorageTexture1D -> storageClassUniformConstant
        BStorageTexture2D -> storageClassUniformConstant
        BStorageTexture2DArray -> storageClassUniformConstant
        BStorageTexture3D -> storageClassUniformConstant
      (baseId, st1) = emitTypeFromLayout st layout
  in emitPointerType st1 storageClass baseId

emitConstU32 :: GenState -> Word32 -> (Word32, GenState)
emitConstU32 st val =
  emitConst st (ConstU32 val) $ \cid st1 ->
    let (u32Id, st2) = emitIntType False st1
        instr = Instr opConstant [u32Id, cid, val]
    in (instr, st2)

emitConstI32 :: GenState -> Word32 -> (Word32, GenState)
emitConstI32 st val =
  emitConst st (ConstI32 val) $ \cid st1 ->
    let (i32Id, st2) = emitIntType True st1
        instr = Instr opConstant [i32Id, cid, val]
    in (instr, st2)

emitTypeConstU32 :: GenState -> Word32 -> (Word32, GenState)
emitTypeConstU32 st val =
  let (u32Id, st1) = emitIntType False st
      (cid, st2) = freshId st1
      instr = Instr opConstant [u32Id, cid, val]
      st3 = addType instr st2
  in (cid, st3)

emitConstF32Bits :: GenState -> Word32 -> (Word32, GenState)
emitConstF32Bits st bits =
  emitConst st (ConstF32 bits) $ \cid st1 ->
    let (f32Id, st2) = emitFloatType st1
        instr = Instr opConstant [f32Id, cid, bits]
    in (instr, st2)

emitConstF32 :: GenState -> Float -> (Word32, GenState)
emitConstF32 st val =
  emitConstF32Bits st (castFloatToWord32 val)

emitConstF16Bits :: GenState -> Word16 -> (Word32, GenState)
emitConstF16Bits st bits16 =
  emitConst st (ConstF16 bits16) $ \cid st1 ->
    let (f16Id, st2) = emitFloatTypeWidth 16 st1
        instr = Instr opConstant [f16Id, cid, fromIntegral bits16]
    in (instr, st2)

emitConstF16 :: GenState -> Float -> (Word32, GenState)
emitConstF16 st val =
  emitConstF16Bits st (floatToHalfBits val)

emitConstBool :: GenState -> Bool -> (Word32, GenState)
emitConstBool st val =
  emitConst st (ConstBool val) $ \cid st1 ->
    let (boolId, st2) = emitBoolType st1
        instr = Instr (if val then opConstantTrue else opConstantFalse) [boolId, cid]
    in (instr, st2)

emitConstComposite :: TypeLayout -> [Word32] -> GenState -> (Word32, GenState)
emitConstComposite layout parts st =
  let (tyId, st1) = emitTypeFromLayout st layout
      (cid, st2) = freshId st1
      instr = Instr opConstantComposite (tyId : cid : parts)
      st3 = addConst instr st2
      st4 = st3 { gsConstComposites = Map.insert cid (layout, parts) st3.gsConstComposites }
  in (cid, st4)

emitSpecConstBool :: GenState -> Bool -> (Word32, GenState)
emitSpecConstBool st val =
  let (boolId, st1) = emitBoolType st
      (cid, st2) = freshId st1
      instr = Instr (if val then opSpecConstantTrue else opSpecConstantFalse) [boolId, cid]
      st3 = addConst instr st2
  in (cid, st3)

emitSpecConstComposite :: TypeLayout -> [Word32] -> GenState -> (Word32, GenState)
emitSpecConstComposite layout parts st =
  let (tyId, st1) = emitTypeFromLayout st layout
      (cid, st2) = freshId st1
      instr = Instr opSpecConstantComposite (tyId : cid : parts)
      st3 = addConst instr st2
      st4 = st3 { gsConstComposites = Map.insert cid (layout, parts) st3.gsConstComposites }
  in (cid, st4)


emitExtInst :: TypeLayout -> Word32 -> [Word32] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitExtInst layout inst args st fs = do
  let (setId, st1) = getExtInstSet st "GLSL.std.450"
  let (tyId, st2) = emitTypeFromLayout st1 layout
  let (resId, st3) = freshId st2
  let instr = Instr opExtInst (tyId : resId : setId : inst : args)
  let fs1 = addFuncInstr instr fs
  Right (st3, fs1, Value layout resId)

emitConstFloatScalar :: Scalar -> Float -> GenState -> Either CompileError (Word32, GenState)
emitConstFloatScalar scalar val st =
  case scalar of
    F32 -> Right (emitConstF32 st val)
    F16 -> Right (emitConstF16 st val)
    _ -> Left (CompileError "expected f16 or f32 scalar" Nothing Nothing)

emitConstIntScalar :: Scalar -> Integer -> GenState -> Either CompileError (Word32, GenState)
emitConstIntScalar scalar val st =
  case scalar of
    I32 ->
      if val < fromIntegral (minBound :: Int32) || val > fromIntegral (maxBound :: Int32)
        then Left (CompileError "integer literal is out of range for i32" Nothing Nothing)
        else Right (emitConstI32 st (fromIntegral val))
    U32 ->
      if val < 0 || val > fromIntegral (maxBound :: Word32)
        then Left (CompileError "integer literal is out of range for u32" Nothing Nothing)
        else Right (emitConstU32 st (fromIntegral val))
    _ -> Left (CompileError "expected i32 or u32 scalar" Nothing Nothing)

emitSpecConstFloatScalar :: Scalar -> Float -> GenState -> Either CompileError (Word32, GenState)
emitSpecConstFloatScalar scalar val st =
  case scalar of
    F32 ->
      let (f32Id, st1) = emitFloatType st
          (cid, st2) = freshId st1
          instr = Instr opSpecConstant [f32Id, cid, castFloatToWord32 val]
          st3 = addConst instr st2
      in Right (cid, st3)
    F16 ->
      let (f16Id, st1) = emitFloatTypeWidth 16 st
          (cid, st2) = freshId st1
          instr = Instr opSpecConstant [f16Id, cid, fromIntegral (floatToHalfBits val)]
          st3 = addConst instr st2
      in Right (cid, st3)
    _ -> Left (CompileError "expected f16 or f32 scalar" Nothing Nothing)

emitSpecConstIntScalar :: Scalar -> Integer -> GenState -> Either CompileError (Word32, GenState)
emitSpecConstIntScalar scalar val st =
  case scalar of
    I32 ->
      if val < fromIntegral (minBound :: Int32) || val > fromIntegral (maxBound :: Int32)
        then Left (CompileError "integer literal is out of range for i32" Nothing Nothing)
        else
          let (i32Id, st1) = emitIntType True st
              (cid, st2) = freshId st1
              instr = Instr opSpecConstant [i32Id, cid, fromIntegral (fromIntegral val :: Int32)]
              st3 = addConst instr st2
          in Right (cid, st3)
    U32 ->
      if val < 0 || val > fromIntegral (maxBound :: Word32)
        then Left (CompileError "integer literal is out of range for u32" Nothing Nothing)
        else
          let (u32Id, st1) = emitIntType False st
              (cid, st2) = freshId st1
              instr = Instr opSpecConstant [u32Id, cid, fromIntegral val]
              st3 = addConst instr st2
          in Right (cid, st3)
    _ -> Left (CompileError "expected i32 or u32 scalar" Nothing Nothing)


emitConstIntSplat :: TypeLayout -> Integer -> GenState -> Either CompileError (GenState, Value)
emitConstIntSplat layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      (cid, st1) <- emitConstIntScalar scalar val st
      Right (st1, Value layout cid)
    TLVector n scalar _ _ -> do
      (cid, st1) <- emitConstIntScalar scalar val st
      let (resId, st2) = emitConstComposite layout (replicate n cid) st1
      Right (st2, Value layout resId)
    _ -> Left (CompileError "expected integer scalar or vector layout" Nothing Nothing)

emitConstFloatSplat :: TypeLayout -> Float -> GenState -> Either CompileError (GenState, Value)
emitConstFloatSplat layout val st =
  case layout of
    TLScalar scalar _ _ -> do
      (cid, st1) <- emitConstFloatScalar scalar val st
      Right (st1, Value layout cid)
    TLVector n scalar _ _ -> do
      (cid, st1) <- emitConstFloatScalar scalar val st
      let (resId, st2) = emitConstComposite layout (replicate n cid) st1
      Right (st2, Value layout resId)
    _ -> Left (CompileError "expected float scalar or vector layout" Nothing Nothing)

emitSplatVector :: TypeLayout -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitSplatVector layout scalarId st fs =
  case layout of
    TLVector n _ _ _ -> do
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let comps = replicate n scalarId
      let fs1 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : comps)) fs
      Right (st2, fs1, Value layout resId)
    _ -> Left (CompileError "expected vector layout for splat" Nothing Nothing)

emitConst :: GenState -> ConstKey -> (Word32 -> GenState -> (Instr, GenState)) -> (Word32, GenState)
emitConst = emitConstWith addConst

emitConstWith :: (Instr -> GenState -> GenState) -> GenState -> ConstKey -> (Word32 -> GenState -> (Instr, GenState)) -> (Word32, GenState)
emitConstWith addInstrFn st key build =
  case Map.lookup key (st.gsConstCache) of
    Just cid -> (cid, st)
    Nothing ->
      let (id', st1) = freshId st
          (instr, st2) = build id' st1
          st3 = addInstrFn instr st2
          st4 = st3
            { gsConstCache = Map.insert key id' st3.gsConstCache
            , gsConstKeyById = Map.insert id' key st3.gsConstKeyById
            }
      in (id', st4)

emitTypeCached :: GenState -> TypeKey -> Instr -> (Word32, GenState)
emitTypeCached st key instr =
  case Map.lookup key (st.gsTypeCache) of
    Just tid -> (tid, st)
    Nothing ->
      let (tid, st1) = freshId st
          st2 = addType (addResultId tid instr) st1
          st3 = st2 { gsTypeCache = Map.insert key tid st2.gsTypeCache }
      in (tid, st3)

addResultId :: Word32 -> Instr -> Instr
addResultId rid (Instr opcode ops) =
  Instr opcode (rid : ops)

-- Type cache key

data TypeKey
  = TKVoid
  | TKBool
  | TKInt Bool Word32
  | TKFloat Word32
  | TKVector Word32 Word32
  | TKMatrix Word32 Word32 Scalar
  | TKSampler
  | TKImage1D Scalar
  | TKImage1DArray Scalar
  | TKImage2D Scalar
  | TKImage2DArray Scalar
  | TKImage3D Scalar
  | TKImageCube Scalar
  | TKImageCubeArray Scalar
  | TKImage2DMultisampled Scalar
  | TKImageDepth2D
  | TKImageDepth2DArray
  | TKImageDepthCube
  | TKImageDepthCubeArray
  | TKImageDepth2DMultisampled
  | TKStorageImage1D StorageFormat
  | TKStorageImage2D StorageFormat
  | TKStorageImage2DArray StorageFormat
  | TKStorageImage3D StorageFormat
  | TKSampledImage Word32
  | TKArray Word32 (Maybe Word32) Word32
  | TKPointer Word32 Word32
  | TKFunction Word32 [Word32]
  deriving (Eq, Ord, Show)

data ConstKey
  = ConstU32 Word32
  | ConstI32 Word32
  | ConstF32 Word32
  | ConstF16 Word16
  | ConstBool Bool
  deriving (Eq, Ord, Show)

bytesToExp :: ByteString -> Q Exp
bytesToExp bytes = do
  let len = BS.length bytes
      lit = TH.LitE (TH.StringPrimL (BS.unpack bytes))
  pure
    (TH.AppE
      (TH.VarE 'unsafeDupablePerformIO)
      (TH.AppE
        (TH.AppE (TH.VarE 'BSI.unsafePackLenAddress) (TH.LitE (TH.IntegerL (fromIntegral len))))
        lit))

interfaceToExp :: ShaderInterface -> Q Exp
interfaceToExp iface = do
  bytesExp <- bytesToExp (BSC.pack (show iface))
  pure (TH.AppE (TH.VarE 'decodeInterface) bytesExp)

decodeInterface :: ByteString -> ShaderInterface
decodeInterface bytes =
  case readMaybe (BSC.unpack bytes) of
    Just iface -> iface
    Nothing -> error "wesl: failed to decode ShaderInterface"

storageFormatCon :: StorageFormat -> TH.Name
storageFormatCon fmt = case fmt of
  FormatRgba8Unorm -> 'FormatRgba8Unorm
  FormatRgba8Snorm -> 'FormatRgba8Snorm
  FormatRgba8Uint -> 'FormatRgba8Uint
  FormatRgba8Sint -> 'FormatRgba8Sint
  FormatRgba16Float -> 'FormatRgba16Float
  FormatRgba16Uint -> 'FormatRgba16Uint
  FormatRgba16Sint -> 'FormatRgba16Sint
  FormatRgba16Unorm -> 'FormatRgba16Unorm
  FormatRgba16Snorm -> 'FormatRgba16Snorm
  FormatRgba32Float -> 'FormatRgba32Float
  FormatRgba32Uint -> 'FormatRgba32Uint
  FormatRgba32Sint -> 'FormatRgba32Sint
  FormatR32Float -> 'FormatR32Float
  FormatR32Uint -> 'FormatR32Uint
  FormatR32Sint -> 'FormatR32Sint
  FormatR16Float -> 'FormatR16Float
  FormatR16Uint -> 'FormatR16Uint
  FormatR16Sint -> 'FormatR16Sint
  FormatR16Unorm -> 'FormatR16Unorm
  FormatR16Snorm -> 'FormatR16Snorm
  FormatR8Unorm -> 'FormatR8Unorm
  FormatR8Snorm -> 'FormatR8Snorm
  FormatR8Uint -> 'FormatR8Uint
  FormatR8Sint -> 'FormatR8Sint
  FormatRg32Float -> 'FormatRg32Float
  FormatRg32Uint -> 'FormatRg32Uint
  FormatRg32Sint -> 'FormatRg32Sint
  FormatRg16Float -> 'FormatRg16Float
  FormatRg16Uint -> 'FormatRg16Uint
  FormatRg16Sint -> 'FormatRg16Sint
  FormatRg16Unorm -> 'FormatRg16Unorm
  FormatRg16Snorm -> 'FormatRg16Snorm
  FormatRg8Unorm -> 'FormatRg8Unorm
  FormatRg8Snorm -> 'FormatRg8Snorm
  FormatRg8Uint -> 'FormatRg8Uint
  FormatRg8Sint -> 'FormatRg8Sint
  FormatRgb10a2Unorm -> 'FormatRgb10a2Unorm
  FormatRgb10a2Uint -> 'FormatRgb10a2Uint
  FormatRg11b10Float -> 'FormatRg11b10Float

storageAccessCon :: StorageAccess -> TH.Name
storageAccessCon access = case access of
  StorageRead -> 'StorageRead
  StorageWrite -> 'StorageWrite
  StorageReadWrite -> 'StorageReadWrite

interfaceToType :: ShaderInterface -> Either String TH.Type
interfaceToType (ShaderInterface bindings _ _ _ _) =
  foldrM
    (\b acc -> do
      bTy <- bindingToType b
      pure (TH.AppT (TH.AppT TH.PromotedConsT bTy) acc))
    TH.PromotedNilT
    bindings

bindingToType :: BindingInfo -> Either String TH.Type
bindingToType info = do
  ty <- typeLayoutToType info.biType
  pure $
    foldl'
      TH.AppT
      (TH.PromotedT 'Binding)
      [ TH.LitT (TH.StrTyLit info.biName)
      , TH.PromotedT (bindingKindTypeCon info.biKind)
      , TH.LitT (TH.NumTyLit (fromIntegral info.biGroup))
      , TH.LitT (TH.NumTyLit (fromIntegral info.biBinding))
      , ty
      ]

bindingKindTypeCon :: BindingKind -> TH.Name
bindingKindTypeCon kind = case kind of
  BUniform -> 'BUniform
  BStorageRead -> 'BStorageRead
  BStorageReadWrite -> 'BStorageReadWrite
  BSampler -> 'BSampler
  BSamplerComparison -> 'BSamplerComparison
  BTexture1D -> 'BTexture1D
  BTexture1DArray -> 'BTexture1DArray
  BTexture2D -> 'BTexture2D
  BTexture2DArray -> 'BTexture2DArray
  BTexture3D -> 'BTexture3D
  BTextureCube -> 'BTextureCube
  BTextureCubeArray -> 'BTextureCubeArray
  BTextureMultisampled2D -> 'BTextureMultisampled2D
  BTextureDepth2D -> 'BTextureDepth2D
  BTextureDepth2DArray -> 'BTextureDepth2DArray
  BTextureDepthCube -> 'BTextureDepthCube
  BTextureDepthCubeArray -> 'BTextureDepthCubeArray
  BTextureDepthMultisampled2D -> 'BTextureDepthMultisampled2D
  BStorageTexture1D -> 'BStorageTexture1D
  BStorageTexture2D -> 'BStorageTexture2D
  BStorageTexture2DArray -> 'BStorageTexture2DArray
  BStorageTexture3D -> 'BStorageTexture3D

typeLayoutToType :: TypeLayout -> Either String TH.Type
typeLayoutToType layout =
  case layout of
    TLScalar s _ _ -> Right (TH.AppT (TH.PromotedT 'TScalar) (scalarTypeToType s))
    TLVector n s _ _ ->
      Right $
        TH.AppT
          (TH.AppT (TH.PromotedT 'TVec) (TH.LitT (TH.NumTyLit (fromIntegral n))))
          (scalarTypeToType s)
    TLMatrix cols rows s _ _ _ ->
      Right $
        TH.AppT
          (TH.AppT (TH.AppT (TH.PromotedT 'TMatrix) (TH.LitT (TH.NumTyLit (fromIntegral cols)))) (TH.LitT (TH.NumTyLit (fromIntegral rows))))
          (scalarTypeToType s)
    TLArray mlen _ elemLayout _ _ -> do
      elemTy <- typeLayoutToType elemLayout
      pure $ case mlen of
        Nothing -> TH.AppT (TH.PromotedT 'TRuntimeArray) elemTy
        Just n -> TH.AppT (TH.AppT (TH.PromotedT 'TArray) (TH.LitT (TH.NumTyLit (fromIntegral n)))) elemTy
    TLStruct _ fields _ _ -> do
      fieldTypes <- foldrM (\f acc -> do ty <- fieldLayoutToType f; pure (TH.AppT (TH.AppT TH.PromotedConsT ty) acc)) TH.PromotedNilT fields
      pure (TH.AppT (TH.PromotedT 'TStruct) fieldTypes)
    TLPointer {} -> Left "pointer types are not supported in interface reflection"
    TLSampler -> Right (TH.PromotedT 'TSampler)
    TLSamplerComparison -> Right (TH.PromotedT 'TSamplerComparison)
    TLTexture1D s -> Right (TH.AppT (TH.PromotedT 'TTexture1D) (scalarTypeToType s))
    TLTexture1DArray s -> Right (TH.AppT (TH.PromotedT 'TTexture1DArray) (scalarTypeToType s))
    TLTexture2D s -> Right (TH.AppT (TH.PromotedT 'TTexture2D) (scalarTypeToType s))
    TLTexture2DArray s -> Right (TH.AppT (TH.PromotedT 'TTexture2DArray) (scalarTypeToType s))
    TLTexture3D s -> Right (TH.AppT (TH.PromotedT 'TTexture3D) (scalarTypeToType s))
    TLTextureCube s -> Right (TH.AppT (TH.PromotedT 'TTextureCube) (scalarTypeToType s))
    TLTextureCubeArray s -> Right (TH.AppT (TH.PromotedT 'TTextureCubeArray) (scalarTypeToType s))
    TLTextureMultisampled2D s -> Right (TH.AppT (TH.PromotedT 'TTextureMultisampled2D) (scalarTypeToType s))
    TLTextureDepth2D -> Right (TH.PromotedT 'TTextureDepth2D)
    TLTextureDepth2DArray -> Right (TH.PromotedT 'TTextureDepth2DArray)
    TLTextureDepthCube -> Right (TH.PromotedT 'TTextureDepthCube)
    TLTextureDepthCubeArray -> Right (TH.PromotedT 'TTextureDepthCubeArray)
    TLTextureDepthMultisampled2D -> Right (TH.PromotedT 'TTextureDepthMultisampled2D)
    TLStorageTexture1D fmt access ->
      Right (TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture1D) (storageFormatToType fmt)) (storageAccessToType access))
    TLStorageTexture2D fmt access ->
      Right (TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture2D) (storageFormatToType fmt)) (storageAccessToType access))
    TLStorageTexture2DArray fmt access ->
      Right (TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture2DArray) (storageFormatToType fmt)) (storageAccessToType access))
    TLStorageTexture3D fmt access ->
      Right (TH.AppT (TH.AppT (TH.PromotedT 'TStorageTexture3D) (storageFormatToType fmt)) (storageAccessToType access))
    TLAtomic s -> Right (TH.AppT (TH.PromotedT 'TAtomic) (scalarTypeToType s))

fieldLayoutToType :: FieldLayout -> Either String TH.Type
fieldLayoutToType fld = do
  ty <- typeLayoutToType fld.flType
  pure $
    TH.AppT
      (TH.AppT (TH.PromotedT 'Field) (TH.LitT (TH.StrTyLit fld.flName)))
      ty

scalarTypeToType :: Scalar -> TH.Type
scalarTypeToType s =
  TH.PromotedT $ case s of
    I32 -> 'SI32
    U32 -> 'SU32
    F16 -> 'SF16
    F32 -> 'SF32
    Bool -> 'SBool

storageFormatToType :: StorageFormat -> TH.Type
storageFormatToType fmt = TH.PromotedT (storageFormatCon fmt)

storageAccessToType :: StorageAccess -> TH.Type
storageAccessToType access = TH.PromotedT (storageAccessCon access)
