{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typechecking, validation, and import resolution.
module Spirdo.Wesl.Typecheck where

import Control.Monad (foldM, zipWithM, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import Spirdo.Wesl.Parser (parseModuleWith)
import Spirdo.Wesl.Syntax
import Spirdo.Wesl.Types
import Spirdo.Wesl.Util
import System.Directory (doesFileExist)
import System.FilePath (dropExtension, makeRelative, splitDirectories, takeDirectory, (<.>), (</>))

validateEntry :: EntryPoint -> Either CompileError ()
validateEntry entry =
  case epStage entry of
    StageCompute ->
      case epWorkgroupSize entry of
        Nothing -> Left (CompileError "@workgroup_size is required for @compute" Nothing Nothing)
        Just _ -> pure ()
    StageVertex ->
      case epWorkgroupSize entry of
        Nothing -> pure ()
        Just _ -> Left (CompileError "@workgroup_size is not allowed for @vertex" Nothing Nothing)
    StageFragment ->
      case epWorkgroupSize entry of
        Nothing -> pure ()
        Just _ -> Left (CompileError "@workgroup_size is not allowed for @fragment" Nothing Nothing)

-- Import resolution (subset: module imports + qualified names)

data ModuleNode = ModuleNode
  { mnFile :: !FilePath
  , mnPath :: ![Text]
  , mnAst :: !ModuleAst
  , mnImports :: ![ImportResolved]
  } deriving (Eq, Show)

data ImportResolved = ImportResolved
  { irModulePath :: ![Text]
  , irModuleFile :: !FilePath
  , irItem :: !(Maybe Text)
  , irAlias :: !(Maybe Text)
  } deriving (Eq, Show)

emptyModuleAst :: ModuleAst
emptyModuleAst = ModuleAst [] [] [] [] [] [] [] [] [] [] Nothing

resolveImports :: CompileOptions -> FilePath -> ModuleAst -> IO (Either CompileError ModuleAst)
resolveImports opts rootFile rootAst = do
  let rootDir = takeDirectory rootFile
  graph <- loadModuleGraph opts rootDir rootFile rootAst
  pure (graph >>= linkModules opts rootFile rootDir)

loadModuleGraph :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> IO (Either CompileError [ModuleNode])
loadModuleGraph opts rootDir rootFile rootAst = go Map.empty [(rootFile, rootAst)]
  where
    go acc [] = pure (Right (Map.elems acc))
    go acc ((filePath, ast):queue) = do
      let pathSegs = modulePathFromFile rootDir filePath
      resolvedImports <- resolveImportItems opts rootDir filePath ast
      case resolvedImports of
        Left err -> pure (Left err)
        Right imports -> do
          case validateImportAliases imports of
            Left err -> pure (Left err)
            Right () -> do
              let node = ModuleNode filePath pathSegs ast imports
              let acc' = Map.insert filePath node acc
              loadTargets <- loadImportTargets opts acc' imports
              case loadTargets of
                Left err -> pure (Left err)
                Right targets -> go acc' (queue <> targets)

    loadImportTargets opts' acc' imports = do
      let moduleFiles = Map.keys (Map.fromList [(irModuleFile imp, ()) | imp <- imports])
      results <- mapM (loadOne opts' acc') moduleFiles
      pure (fmap concat (sequence results))

    loadOne opts' acc' moduleFile =
      if Map.member moduleFile acc'
        then pure (Right [])
        else do
          moduleAst <- loadModuleFromFile opts' moduleFile
          pure (fmap (\ast' -> [(moduleFile, ast')]) moduleAst)

validateImportAliases :: [ImportResolved] -> Either CompileError ()
validateImportAliases imports =
  let aliases = mapMaybe aliasFor imports
      (dups, _) = foldl' collect ([], Set.empty) aliases
      targets = map importTarget imports
      (dupTargets, _) = foldl' collect ([], Set.empty) targets
  in if not (null dups)
      then Left (CompileError ("duplicate import aliases: " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
      else
        if null dupTargets
          then Right ()
          else Left (CompileError ("duplicate imports: " <> T.unpack (T.intercalate ", " dupTargets)) Nothing Nothing)
  where
    aliasFor imp =
      case irItem imp of
        Nothing ->
          let alias = fromMaybe (last (irModulePath imp)) (irAlias imp)
          in if T.null alias then Nothing else Just alias
        Just item ->
          let alias = fromMaybe item (irAlias imp)
          in if T.null alias then Nothing else Just alias

    importTarget imp =
      let modName = T.intercalate "::" (irModulePath imp)
      in case irItem imp of
          Nothing -> modName
          Just item -> modName <> "::" <> item

    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)

resolveImportItems :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> IO (Either CompileError [ImportResolved])
resolveImportItems opts rootDir moduleFile ast = do
  let items = [(decl, item) | decl <- modImports ast, item <- idItems decl]
  results <- mapM (resolveOne opts rootDir moduleFile) items
  pure (sequence results)
  where
    resolveOne opts' rootDir' moduleFile' (decl, item) = resolveImportItem opts' rootDir' moduleFile' decl item

loadModuleFromFile :: CompileOptions -> FilePath -> IO (Either CompileError ModuleAst)
loadModuleFromFile opts path = do
  let candidateWesl = path <.> "wesl"
  let candidateWgsl = path <.> "wgsl"
  weslExists <- doesFileExist candidateWesl
  if weslExists
    then parseModule candidateWesl
    else do
      wgslExists <- doesFileExist candidateWgsl
      if wgslExists
        then parseModule candidateWgsl
        else pure (Right emptyModuleAst)
  where
    parseModule file = do
      src <- readFile file
      pure (parseModuleWith (enabledFeatures opts) src)

resolveImportItem :: CompileOptions -> FilePath -> FilePath -> ImportDecl -> ImportItem -> IO (Either CompileError ImportResolved)
resolveImportItem opts rootDir moduleFile decl item = do
  let baseDir = importBaseDir rootDir moduleFile (idRelative decl)
  let segs = iiPath item
  let fullBase = foldl (</>) baseDir (map T.unpack segs)
  fullMod <- findModuleFile fullBase
  case fullMod of
    Just moduleBase -> do
      ambiguous <- case segs of
        [] -> pure False
        [_] -> pure False
        _ -> do
          let modSegs = init segs
          let itemName = last segs
          let moduleBasePath = foldl (</>) baseDir (map T.unpack modSegs)
          moduleBaseItem <- findModuleFile moduleBasePath
          case moduleBaseItem of
            Nothing -> pure False
            Just mb -> do
              itemExists <- moduleHasItem opts mb itemName
              pure itemExists
      if ambiguous
        then pure (Left (CompileError ("ambiguous import: " <> T.unpack (T.intercalate "::" segs) <> " refers to both a module and an item") Nothing Nothing))
        else pure (Right (ImportResolved (modulePathFromFile rootDir moduleBase) moduleBase Nothing (iiAlias item)))
    Nothing ->
      case segs of
        [] -> pure (Left (CompileError "import path is empty" Nothing Nothing))
        [_] -> pure (Left (CompileError ("import module not found: " <> T.unpack (T.intercalate "::" segs)) Nothing Nothing))
        _ -> do
          let modSegs = init segs
          let itemName = last segs
          let moduleBasePath = foldl (</>) baseDir (map T.unpack modSegs)
          moduleBase <- findModuleFile moduleBasePath
          case moduleBase of
            Just mb ->
              pure (Right (ImportResolved (modulePathFromFile rootDir mb) mb (Just itemName) (iiAlias item)))
            Nothing ->
              pure (Left (CompileError ("import module not found: " <> T.unpack (T.intercalate "::" modSegs)) Nothing Nothing))

moduleHasItem :: CompileOptions -> FilePath -> Text -> IO Bool
moduleHasItem opts moduleBase itemName = do
  astResult <- loadModuleFromFile opts moduleBase
  case astResult of
    Left _ -> pure False
    Right ast ->
      pure $
        itemName `elem` (map sdName (modStructs ast))
          || itemName `elem` map bdName (modBindings ast)
          || itemName `elem` map gvName (modGlobals ast)
          || itemName `elem` map cdName (modConsts ast)
          || itemName `elem` map odName (modOverrides ast)
          || itemName `elem` map adName (modAliases ast)
          || itemName `elem` map fnName (modFunctions ast)
          || maybe False ((== itemName) . epName) (modEntry ast)

findModuleFile :: FilePath -> IO (Maybe FilePath)
findModuleFile base = do
  let weslPath = base <.> "wesl"
  let wgslPath = base <.> "wgsl"
  weslExists <- doesFileExist weslPath
  if weslExists
    then pure (Just base)
    else do
      wgslExists <- doesFileExist wgslPath
      if wgslExists
        then pure (Just base)
        else pure Nothing

modulePathFromFile :: FilePath -> FilePath -> [Text]
modulePathFromFile rootDir filePath =
  let rel = dropExtension (makeRelative rootDir filePath)
  in map T.pack (filter (not . null) (splitDirectories rel))

importBaseDir :: FilePath -> FilePath -> Maybe ImportRelative -> FilePath
importBaseDir rootDir moduleFile rel =
  case rel of
    Nothing -> rootDir
    Just ImportPackage -> rootDir
    Just (ImportSuper n) -> superModuleFile moduleFile n

superModuleFile :: FilePath -> Int -> FilePath
superModuleFile filePath n =
  iterate takeDirectory filePath !! n

linkModules :: CompileOptions -> FilePath -> FilePath -> [ModuleNode] -> Either CompileError ModuleAst
linkModules opts rootFile rootDir nodes = do
  rootNode <- case [n | n <- nodes, mnFile n == rootFile] of
    (n:_) -> Right n
    [] -> Left (CompileError "root module not found during import resolution" Nothing Nothing)
  let rootPath = mnPath rootNode
  let otherEntries = [n | n <- nodes, mnFile n /= rootFile, modEntry (mnAst n) /= Nothing]
  when (not (null otherEntries)) $
    Left (CompileError "entry points are only supported in the root module" Nothing Nothing)
  let constIndex = buildConstIndex nodes
  let fnIndex = buildFunctionIndex nodes
  let structIndex = buildStructIndex nodes
  let overrideIndex = buildOverrideIndex nodes
  validateModuleScopes opts True rootPath rootDir constIndex fnIndex structIndex overrideIndex nodes
  let contexts = Map.fromList [(mnFile n, buildModuleContext rootPath rootDir n) | n <- nodes]
  let qualified = map (\n -> qualifyModule rootPath contexts n) nodes
  let merged = foldl' mergeModule emptyModuleAst qualified
  let rootEntry =
        case [q | q <- qualified, mnFile q == rootFile] of
          (q:_) -> modEntry (mnAst q)
          [] -> Nothing
  Right merged { modEntry = rootEntry }

mergeModule :: ModuleAst -> ModuleNode -> ModuleAst
mergeModule acc node =
  let ast = mnAst node
  in acc
      { modDirectives = modDirectives acc <> modDirectives ast
      , modAliases = modAliases acc <> modAliases ast
      , modStructs = modStructs acc <> modStructs ast
      , modBindings = modBindings acc <> modBindings ast
      , modGlobals = modGlobals acc <> modGlobals ast
      , modConsts = modConsts acc <> modConsts ast
      , modOverrides = modOverrides acc <> modOverrides ast
      , modConstAsserts = modConstAsserts acc <> modConstAsserts ast
      , modFunctions = modFunctions acc <> modFunctions ast
      }

data ModuleContext = ModuleContext
  { mcPath :: [Text]
  , mcModuleAliases :: Map.Map Text [Text]
  , mcItemAliases :: Map.Map Text [Text]
  , mcLocals :: Set.Set Text
  , mcConstNames :: Set.Set Text
  , mcFunctionNames :: Set.Set Text
  , mcRootPath :: [Text]
  }

data NameTable = NameTable
  { ntNext :: !Int
  , ntMap :: !(Map.Map Text Int)
  } deriving (Eq, Show)

emptyNameTable :: NameTable
emptyNameTable = NameTable 0 Map.empty

internName :: Text -> NameTable -> (Int, NameTable)
internName name table =
  case Map.lookup name (ntMap table) of
    Just i -> (i, table)
    Nothing ->
      let i = ntNext table
          table' = table { ntNext = i + 1, ntMap = Map.insert name i (ntMap table) }
      in (i, table')

internNames :: NameTable -> [Text] -> (NameTable, [Int])
internNames table names =
  let (table', revIds) = foldl' step (table, []) names
  in (table', reverse revIds)
  where
    step (t, acc) name =
      let (i, t') = internName name t
      in (t', i : acc)

internNameSet :: NameTable -> [Text] -> (NameTable, IntSet.IntSet)
internNameSet table names =
  let (table', ids) = internNames table names
  in (table', IntSet.fromList ids)

internNamePairs :: NameTable -> [Text] -> (NameTable, [(Text, Int)])
internNamePairs table names =
  let (table', ids) = internNames table names
  in (table', zip names ids)

uniqueById :: [(Text, Int)] -> [(Text, Int)]
uniqueById = go IntSet.empty
  where
    go _ [] = []
    go seen ((name, ident) : rest)
      | IntSet.member ident seen = go seen rest
      | otherwise = (name, ident) : go (IntSet.insert ident seen) rest

pathKey :: [Text] -> Text
pathKey = T.intercalate "::"

buildModuleContext :: [Text] -> FilePath -> ModuleNode -> ModuleContext
buildModuleContext rootPath _rootDir node =
  let ast = mnAst node
      localNames =
        Set.fromList
          ( map sdName (modStructs ast)
            <> map bdName (modBindings ast)
            <> map gvName (modGlobals ast)
            <> map cdName (modConsts ast)
            <> map odName (modOverrides ast)
            <> map fnName (modFunctions ast)
            <> maybe [] (\e -> [epName e]) (modEntry ast)
          )
      constNames = Set.fromList (map cdName (modConsts ast) <> map odName (modOverrides ast))
      functionNames = Set.fromList (map fnName (modFunctions ast))
      (moduleAliases, itemAliases) = buildAliasMaps (mnImports node)
  in ModuleContext (mnPath node) moduleAliases itemAliases localNames constNames functionNames rootPath

data ConstIndex = ConstIndex
  { ciPathTable :: !NameTable
  , ciNameTable :: !NameTable
  , ciEntries :: !(IntMap.IntMap (IntMap.IntMap Expr))
  }

buildConstIndex :: [ModuleNode] -> ConstIndex
buildConstIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in ConstIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey (mnPath node)) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) (constPairs node)
          merged = IntMap.union entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, expr) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insert nid expr m)
    constPairs n =
      let ast = mnAst n
          consts = [(cdName c, cdExpr c) | c <- modConsts ast]
          overrides = [(odName o, expr) | o <- modOverrides ast, Just expr <- [odExpr o]]
      in consts <> overrides

lookupConstIndex :: ConstIndex -> [Text] -> Text -> Maybe Expr
lookupConstIndex idx path name = do
  pid <- Map.lookup (pathKey path) (ntMap (ciPathTable idx))
  nid <- Map.lookup name (ntMap (ciNameTable idx))
  IntMap.lookup pid (ciEntries idx) >>= IntMap.lookup nid

type OverrideIndex = Map.Map [Text] (Set.Set Text)

buildOverrideIndex :: [ModuleNode] -> OverrideIndex
buildOverrideIndex nodes =
  Map.fromList
    [ (mnPath n, Set.fromList (map odName (modOverrides (mnAst n))))
    | n <- nodes
    ]

data StructIndex = StructIndex
  { siPathTable :: !NameTable
  , siNameTable :: !NameTable
  , siEntries :: !(IntMap.IntMap (IntMap.IntMap StructDecl))
  }

buildStructIndex :: [ModuleNode] -> StructIndex
buildStructIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in StructIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey (mnPath node)) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) [(sdName s, s) | s <- modStructs (mnAst node)]
          merged = IntMap.union entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, decl) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insert nid decl m)

lookupStructIndex :: StructIndex -> [Text] -> Text -> Maybe StructDecl
lookupStructIndex idx path name = do
  pid <- Map.lookup (pathKey path) (ntMap (siPathTable idx))
  nid <- Map.lookup name (ntMap (siNameTable idx))
  IntMap.lookup pid (siEntries idx) >>= IntMap.lookup nid

data FunctionIndex = FunctionIndex
  { fiPathTable :: !NameTable
  , fiNameTable :: !NameTable
  , fiEntries :: !(IntMap.IntMap (IntMap.IntMap [FunctionDecl]))
  }

buildFunctionIndex :: [ModuleNode] -> FunctionIndex
buildFunctionIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in FunctionIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey (mnPath node)) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) [(fnName f, [f]) | f <- modFunctions (mnAst node)]
          merged = IntMap.unionWith (<>) entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, decls) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insertWith (<>) nid decls m)

lookupFunctionIndex :: FunctionIndex -> [Text] -> Text -> Maybe [FunctionDecl]
lookupFunctionIndex idx path name = do
  pid <- Map.lookup (pathKey path) (ntMap (fiPathTable idx))
  nid <- Map.lookup name (ntMap (fiNameTable idx))
  IntMap.lookup pid (fiEntries idx) >>= IntMap.lookup nid

lowerOverridesWith :: [Text] -> [(Text, OverrideValue)] -> ModuleAst -> Either CompileError ModuleAst
lowerOverridesWith rootPath overridesMap ast =
  case modOverrides ast of
    [] -> Right ast
    overrides -> do
      let existing = Set.fromList (map cdName (modConsts ast))
      let (dups, _) = foldl' collect ([], Set.empty) (map odName overrides)
      when (not (null dups)) $
        Left (CompileError ("duplicate override declarations: " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
      when (any (`Set.member` existing) (map odName overrides)) $
        Left (CompileError "override names must not conflict with const declarations" Nothing Nothing)
      let overrideLookup = buildOverrideValueMap rootPath overridesMap
      let structEnv = [(sdName s, s) | s <- modStructs ast]
      (consts, kept) <- foldM (partitionOverride structEnv overrideLookup) ([], []) overrides
      Right ast { modConsts = modConsts ast <> reverse consts, modOverrides = reverse kept }
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)
    partitionOverride structEnv overrideLookup (constAcc, keepAcc) o =
      case Map.lookup (odName o) overrideLookup of
        Just ov -> do
          expr <- overrideValueToExpr structEnv (odType o) ov
          Right (ConstDecl (odName o) expr : constAcc, keepAcc)
        Nothing -> Right (constAcc, o : keepAcc)

buildOverrideValueMap :: [Text] -> [(Text, OverrideValue)] -> Map.Map Text OverrideValue
buildOverrideValueMap rootPath overrides =
  Map.fromList
    [ (candidate, val)
    | (key, val) <- overrides
    , candidate <- overrideKeyCandidates rootPath key
    ]

overrideKeyCandidates :: [Text] -> Text -> [Text]
overrideKeyCandidates rootPath key
  | "__wesl__" `T.isPrefixOf` key = [key]
  | otherwise =
      case splitQName key of
        [] -> []
        [single] -> [single]
        segs ->
          let path = init segs
              name = last segs
          in if path == rootPath || null rootPath
                then [name]
                else ["__wesl__" <> T.intercalate "__" path <> "__" <> name]

overrideValueToExpr :: [(Text, StructDecl)] -> Type -> OverrideValue -> Either CompileError Expr
overrideValueToExpr structEnv ty ov =
  case ty of
    TyScalar Bool ->
      case ov of
        OVBool b -> Right (EBool b)
        _ -> Left (CompileError "override value must be a bool" Nothing Nothing)
    TyScalar I32 ->
      case ov of
        OVI32 v -> Right (EInt v)
        _ -> Left (CompileError "override value must be an i32" Nothing Nothing)
    TyScalar U32 ->
      case ov of
        OVU32 v -> Right (ECall "u32" [EInt v])
        _ -> Left (CompileError "override value must be a u32" Nothing Nothing)
    TyScalar F32 ->
      case ov of
        OVF32 v -> Right (EFloat v)
        OVF16 v -> Right (ECall "f32" [EFloat v])
        _ -> Left (CompileError "override value must be an f32" Nothing Nothing)
    TyScalar F16 ->
      case ov of
        OVF16 v -> Right (ECall "f16" [EFloat v])
        OVF32 v -> Right (ECall "f16" [EFloat v])
        _ -> Left (CompileError "override value must be an f16" Nothing Nothing)
    TyVector n scalar ->
      case ov of
        OVComposite vals -> do
          when (length vals /= n) $
            Left (CompileError "override vector arity does not match" Nothing Nothing)
          args <- mapM (overrideValueToExpr structEnv (TyScalar scalar)) vals
          Right (ECall ("vec" <> T.pack (show n)) args)
        _ -> Left (CompileError "override value must be a vector" Nothing Nothing)
    TyMatrix cols rows scalar ->
      case ov of
        OVComposite colsVals -> do
          when (length colsVals /= cols) $
            Left (CompileError "override matrix column count does not match" Nothing Nothing)
          let colTy = TyVector rows scalar
          args <- mapM (overrideValueToExpr structEnv colTy) colsVals
          Right (ECall ("mat" <> T.pack (show cols) <> "x" <> T.pack (show rows)) args)
        _ -> Left (CompileError "override value must be a matrix" Nothing Nothing)
    TyArray elemTy (Just n) ->
      case ov of
        OVComposite vals -> do
          when (length vals /= n) $
            Left (CompileError "override array length does not match" Nothing Nothing)
          args <- mapM (overrideValueToExpr structEnv elemTy) vals
          Right (ECall "array" args)
        _ -> Left (CompileError "override value must be an array" Nothing Nothing)
    TyArray _ Nothing ->
      Left (CompileError "override values cannot target runtime-sized arrays" Nothing Nothing)
    TyStructRef name ->
      case lookup name structEnv of
        Nothing -> Left (CompileError ("unknown struct: " <> T.unpack name) Nothing Nothing)
        Just decl ->
          case ov of
            OVComposite vals -> do
              let fields = sdFields decl
              when (length vals /= length fields) $
                Left (CompileError "override struct field count does not match" Nothing Nothing)
              args <- zipWithM (\f v -> overrideValueToExpr structEnv (fdType f) v) fields vals
              Right (ECall name args)
            _ -> Left (CompileError "override value must be a struct composite" Nothing Nothing)
    _ -> Left (CompileError "override values are only supported for scalar, vector, matrix, array, and struct types" Nothing Nothing)

resolveTypeAliases :: ModuleAst -> Either CompileError ModuleAst
resolveTypeAliases ast = do
  let (dupAliases, _) = foldl' collect ([], Set.empty) (map adName (modAliases ast))
  when (not (null dupAliases)) $
    Left (CompileError ("duplicate type aliases: " <> T.unpack (T.intercalate ", " dupAliases)) Nothing Nothing)
  let aliasMap = Map.fromList [(adName a, adType a) | a <- modAliases ast]
  let expand = expandType aliasMap
  aliases <- mapM (\a -> (\ty -> a { adType = ty }) <$> expand (adType a)) (modAliases ast)
  structs <- mapM (expandStruct expand) (modStructs ast)
  bindings <- mapM (expandBinding expand) (modBindings ast)
  globals <- mapM (expandGlobal expand) (modGlobals ast)
  consts <- mapM (expandConst expand) (modConsts ast)
  overrides <- mapM (expandOverride expand) (modOverrides ast)
  constAsserts <- mapM (expandConstAssert expand) (modConstAsserts ast)
  functions <- mapM (expandFunction expand) (modFunctions ast)
  entry <- mapM (expandEntry expand) (modEntry ast)
  Right ast
    { modAliases = aliases
    , modStructs = structs
    , modBindings = bindings
    , modGlobals = globals
    , modConsts = consts
    , modOverrides = overrides
    , modConstAsserts = constAsserts
    , modFunctions = functions
    , modEntry = entry
    }
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)
    expandType aliasMap = go Set.empty
      where
        go stack ty =
          case ty of
            TyStructRef name ->
              case Map.lookup name aliasMap of
                Nothing -> Right ty
                Just aliasTy ->
                  if Set.member name stack
                    then Left (CompileError ("type alias cycle involving: " <> T.unpack name) Nothing Nothing)
                    else go (Set.insert name stack) aliasTy
            TyArray elemTy n -> TyArray <$> go stack elemTy <*> pure n
            _ -> Right ty

    expandStruct expand decl = do
      fields <- mapM (\f -> (\ty -> f { fdType = ty }) <$> expand (fdType f)) (sdFields decl)
      Right decl { sdFields = fields }

    expandBinding expand decl = do
      ty <- expand (bdType decl)
      Right decl { bdType = ty }

    expandGlobal expand decl = do
      ty <- expand (gvType decl)
      init' <- mapM (expandExpr expand) (gvInit decl)
      Right decl { gvType = ty, gvInit = init' }

    expandConstAssert expand (ConstAssert pos expr) = do
      expr' <- expandExpr expand expr
      Right (ConstAssert pos expr')

    expandConst expand decl = do
      expr <- expandExpr expand (cdExpr decl)
      Right decl { cdExpr = expr }

    expandOverride expand decl = do
      ty <- expand (odType decl)
      expr <- mapM (expandExpr expand) (odExpr decl)
      Right decl { odType = ty, odExpr = expr }

    expandFunction expand decl = do
      params <- mapM (\p -> (\ty -> p { paramType = ty }) <$> expand (paramType p)) (fnParams decl)
      ret <- mapM expand (fnReturnType decl)
      body <- mapM (expandStmt expand) (fnBody decl)
      Right decl { fnParams = params, fnReturnType = ret, fnBody = body }

    expandEntry expand decl = do
      params <- mapM (\p -> (\ty -> p { paramType = ty }) <$> expand (paramType p)) (epParams decl)
      ret <- mapM expand (epReturnType decl)
      body <- mapM (expandStmt expand) (epBody decl)
      Right decl { epParams = params, epReturnType = ret, epBody = body }

    expandStmt expand stmt =
      case stmt of
        SLet name expr -> SLet name <$> expandExpr expand expr
        SVar name expr -> SVar name <$> expandExpr expand expr
        SAssign lv expr -> SAssign <$> expandLValue expand lv <*> expandExpr expand expr
        SAssignOp lv op expr -> SAssignOp <$> expandLValue expand lv <*> pure op <*> expandExpr expand expr
        SInc lv -> SInc <$> expandLValue expand lv
        SDec lv -> SDec <$> expandLValue expand lv
        SExpr expr -> SExpr <$> expandExpr expand expr
        SIf cond thenBody elseBody ->
          SIf <$> expandExpr expand cond <*> mapM (expandStmt expand) thenBody <*> mapM (mapM (expandStmt expand)) elseBody
        SWhile cond body -> SWhile <$> expandExpr expand cond <*> mapM (expandStmt expand) body
        SLoop body cont -> SLoop <$> mapM (expandStmt expand) body <*> mapM (mapM (expandStmt expand)) cont
        SFor initStmt cond cont body ->
          SFor <$> mapM (expandStmt expand) initStmt <*> mapM (expandExpr expand) cond <*> mapM (expandStmt expand) cont <*> mapM (expandStmt expand) body
        SSwitch expr cases defBody ->
          SSwitch <$> expandExpr expand expr <*> mapM (expandCase expand) cases <*> mapM (mapM (expandStmt expand)) defBody
        SBreak -> Right SBreak
        SBreakIf expr -> SBreakIf <$> expandExpr expand expr
        SContinue -> Right SContinue
        SDiscard -> Right SDiscard
        SFallthrough -> Right SFallthrough
        SReturn expr -> SReturn <$> mapM (expandExpr expand) expr

    expandCase expand (SwitchCase sels body) =
      SwitchCase <$> mapM (expandExpr expand) sels <*> mapM (expandStmt expand) body

    expandLValue expand lv =
      case lv of
        LVVar name -> Right (LVVar name)
        LVField inner field -> LVField <$> expandLValue expand inner <*> pure field
        LVIndex inner idx -> LVIndex <$> expandLValue expand inner <*> expandExpr expand idx
        LVDeref expr -> LVDeref <$> expandExpr expand expr

    expandExpr expand expr =
      case expr of
        EBinary op a b -> EBinary op <$> expandExpr expand a <*> expandExpr expand b
        EUnary op a -> EUnary op <$> expandExpr expand a
        ECall name args -> ECall name <$> mapM (expandExpr expand) args
        EBitcast ty arg -> EBitcast <$> expand ty <*> expandExpr expand arg
        EField base field -> EField <$> expandExpr expand base <*> pure field
        EIndex base idx -> EIndex <$> expandExpr expand base <*> expandExpr expand idx
        _ -> Right expr

buildAliasMaps :: [ImportResolved] -> (Map.Map Text [Text], Map.Map Text [Text])
buildAliasMaps imports = foldl' add (Map.empty, Map.empty) imports
  where
    add (modAcc, itemAcc) imp =
      case irItem imp of
        Nothing ->
          let alias = fromMaybe (last (irModulePath imp)) (irAlias imp)
          in if T.null alias
              then (modAcc, itemAcc)
              else (Map.insert alias (irModulePath imp) modAcc, itemAcc)
        Just item ->
          let alias = fromMaybe item (irAlias imp)
              target = irModulePath imp <> [item]
          in if T.null alias
              then (modAcc, itemAcc)
              else (modAcc, Map.insert alias target itemAcc)

data Scope = Scope
  { scNameTable :: !NameTable
  , scGlobals :: IntSet.IntSet
  , scScopes :: [IntSet.IntSet]
  , scModuleAliases :: IntSet.IntSet
  , scItemAliases :: IntSet.IntSet
  , scTypeAliases :: IntSet.IntSet
  , scAllowShadowing :: Bool
  , scAllowFallthrough :: Bool
  }

validateDirectives :: CompileOptions -> [Directive] -> Either CompileError DiagnosticConfig
validateDirectives opts directives = do
  mapM_ checkEnable directives
  pure (foldl' applyDiag Map.empty directives)
  where
    enabled = Set.fromList (map T.pack (enabledFeatures opts))
    checkEnable dir =
      case dir of
        DirEnable feat ->
          if Set.member feat enabled
            then Right ()
            else Left (CompileError ("feature not enabled: " <> T.unpack feat) Nothing Nothing)
        _ -> Right ()
    applyDiag acc dir =
      case dir of
        DirDiagnostic severity rule -> Map.insert (T.unpack rule) severity acc
        _ -> acc

diagnosticSeverity :: DiagnosticConfig -> String -> DiagnosticSeverity
diagnosticSeverity cfg rule = fromMaybe DiagError (Map.lookup rule cfg)

validateModuleScopes :: CompileOptions -> Bool -> [Text] -> FilePath -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> [ModuleNode] -> Either CompileError ()
validateModuleScopes opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex nodes =
  mapM_ (validateModuleScope opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex) nodes

validateModuleScope :: CompileOptions -> Bool -> [Text] -> FilePath -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> ModuleNode -> Either CompileError ()
validateModuleScope opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex node = do
  let ctx = buildModuleContext rootPath rootDir node
  diagConfig <- validateDirectives opts (modDirectives (mnAst node))
  let allowShadowing = diagnosticSeverity diagConfig "shadowing" /= DiagError
  let (nt1, globalsIds) = internNameSet emptyNameTable (Set.toList (mcLocals ctx))
  let (nt2, moduleAliasIds) = internNameSet nt1 (Map.keys (mcModuleAliases ctx))
  let (nt3, itemAliasIds) = internNameSet nt2 (Map.keys (mcItemAliases ctx))
  let (nt4, typeAliasIds) = internNameSet nt3 (map adName (modAliases (mnAst node)))
  let scope0 =
        Scope
          { scNameTable = nt4
          , scGlobals = globalsIds
          , scScopes = [IntSet.empty]
          , scModuleAliases = moduleAliasIds
          , scItemAliases = itemAliasIds
          , scTypeAliases = typeAliasIds
          , scAllowShadowing = allowShadowing
          , scAllowFallthrough = False
          }
  validateModuleAst ctx constIndex fnIndex structIndex overrideIndex scope0 diagConfig skipConstAsserts (mnAst node)

type DiagnosticConfig = Map.Map String DiagnosticSeverity

validateModuleAst :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> Scope -> DiagnosticConfig -> Bool -> ModuleAst -> Either CompileError ()
validateModuleAst ctx constIndex fnIndex structIndex overrideIndex scope diagConfig skipConstAsserts ast = do
  mapM_ (validateAlias ctx scope) (modAliases ast)
  mapM_ (validateStruct ctx scope) (modStructs ast)
  mapM_ (validateBinding ctx scope) (modBindings ast)
  mapM_ (validateGlobalVar ctx scope) (modGlobals ast)
  mapM_ (validateConst ctx scope) (modConsts ast)
  mapM_ (validateOverride ctx scope overrideIndex) (modOverrides ast)
  when (not skipConstAsserts) $
    mapM_ (validateConstAssert ctx constIndex fnIndex structIndex diagConfig) (modConstAsserts ast)
  mapM_ (validateFunction ctx constIndex fnIndex structIndex skipConstAsserts scope) (modFunctions ast)
  case modEntry ast of
    Nothing -> Right ()
    Just entry -> validateEntryPoint ctx constIndex fnIndex structIndex skipConstAsserts scope entry

validateConstAssertsMerged :: CompileOptions -> [Text] -> ModuleAst -> Either CompileError ()
validateConstAssertsMerged opts rootPath ast = do
  let node = ModuleNode "<merged>" rootPath ast []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext rootPath "" node
  diagConfig <- validateDirectives opts (modDirectives ast)
  mapM_ (validateConstAssert ctx constIndex fnIndex structIndex diagConfig) (modConstAsserts ast)
  Right ()

collectDiagnosticsMerged :: CompileOptions -> [Text] -> ModuleAst -> Either CompileError [Diagnostic]
collectDiagnosticsMerged opts rootPath ast = do
  let node = ModuleNode "<merged>" rootPath ast []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext rootPath "" node
  diagConfig <- validateDirectives opts (modDirectives ast)
  constDiags <- fmap concat (mapM (collectConstAssertDiagnostic ctx constIndex fnIndex structIndex diagConfig) (modConstAsserts ast))
  let unreachableDiags = collectUnreachableDiagnostics diagConfig ast
  let unusedExprDiags = collectUnusedExpressionDiagnostics diagConfig ast
  let unusedVarDiags = collectUnusedVariableDiagnostics diagConfig ast
  let unusedParamDiags = collectUnusedParameterDiagnostics diagConfig ast
  let shadowingDiags = collectShadowingDiagnostics diagConfig ast
  let constantCondDiags = collectConstantConditionDiagnostics diagConfig ctx constIndex fnIndex structIndex ast
  let duplicateCaseDiags = collectDuplicateCaseDiagnostics diagConfig ctx constIndex fnIndex structIndex ast
  Right
    ( constDiags
        <> unreachableDiags
        <> unusedExprDiags
        <> unusedVarDiags
        <> unusedParamDiags
        <> shadowingDiags
        <> constantCondDiags
        <> duplicateCaseDiags
    )

collectConstAssertDiagnostic :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> DiagnosticConfig -> ConstAssert -> Either CompileError [Diagnostic]
collectConstAssertDiagnostic ctx constIndex fnIndex structIndex diagConfig (ConstAssert pos expr) =
  case diagnosticSeverity diagConfig "const_assert" of
    DiagOff -> Right []
    DiagInfo -> emitIfFalse DiagInfo
    DiagWarning -> emitIfFalse DiagWarning
    DiagError -> do
      ok <- withPos pos (evalConstBoolExpr ctx constIndex fnIndex structIndex expr)
      if ok
        then Right []
        else Left (errorAtPos pos "const_assert condition failed")
  where
    emitIfFalse sev = do
      ok <- withPos pos (evalConstBoolExpr ctx constIndex fnIndex structIndex expr)
      if ok
        then Right []
        else Right [Diagnostic sev "const_assert" "const_assert condition failed" (Just (spLine pos)) (Just (spCol pos))]

collectUnreachableDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnreachableDiagnostics diagConfig ast =
  case Map.lookup "unreachable_code" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let diag = Diagnostic sev "unreachable_code" "unreachable code" Nothing Nothing
          bodies = map fnBody (modFunctions ast) <> maybe [] (pure . epBody) (modEntry ast)
      in concatMap (unreachableInStmts diag) bodies
  where
    unreachableInStmts diag = go False
      where
        go _ [] = []
        go unreachable (stmt:rest) =
          let here = if unreachable then [diag] else []
              nested =
                case stmt of
                  SIf _ thenBody elseBody ->
                    unreachableInStmts diag thenBody <> maybe [] (unreachableInStmts diag) elseBody
                  SWhile _ body ->
                    unreachableInStmts diag body
                  SLoop body continuing ->
                    unreachableInStmts diag body <> maybe [] (unreachableInStmts diag) continuing
                  SFor _ _ _ body ->
                    unreachableInStmts diag body
                  SSwitch _ cases defBody ->
                    concatMap (unreachableInStmts diag . scBody) cases <> maybe [] (unreachableInStmts diag) defBody
                  _ -> []
              unreachable' = unreachable || isTerminator stmt
          in here <> nested <> go unreachable' rest

    isTerminator stmt =
      case stmt of
        SReturn _ -> True
        SBreak -> True
        SContinue -> True
        SDiscard -> True
        _ -> False

collectUnusedExpressionDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedExpressionDiagnostics diagConfig ast =
  case Map.lookup "unused_expression" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let diag = Diagnostic sev "unused_expression" "expression has no effect" Nothing Nothing
          bodies = map fnBody (modFunctions ast) <> maybe [] (pure . epBody) (modEntry ast)
      in concatMap (unusedExprs diag) bodies
  where
    unusedExprs diag = go
      where
        go [] = []
        go (stmt:rest) =
          let here =
                case stmt of
                  SExpr expr ->
                    case expr of
                      ECall _ _ -> []
                      _ -> [diag]
                  _ -> []
              nested =
                case stmt of
                  SIf _ thenBody elseBody ->
                    go thenBody <> maybe [] go elseBody
                  SWhile _ body ->
                    go body
                  SLoop body continuing ->
                    go body <> maybe [] go continuing
                  SFor _ _ _ body ->
                    go body
                  SSwitch _ cases defBody ->
                    concatMap (go . scBody) cases <> maybe [] go defBody
                  _ -> []
          in here <> nested <> go rest

collectUnusedVariableDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedVariableDiagnostics diagConfig ast =
  case Map.lookup "unused_variable" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let diag name = Diagnostic sev "unused_variable" ("unused variable: " <> T.unpack name) Nothing Nothing
          bodies = map fnBody (modFunctions ast) <> maybe [] (pure . epBody) (modEntry ast)
      in concatMap (unusedVarsInBody diag) bodies
  where
    unusedVarsInBody mkDiag stmts =
      let declaredNames = collectDecls stmts
          (nt1, declaredPairs) = internNamePairs emptyNameTable declaredNames
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts stmts)
          unused =
            [ name
            | (name, ident) <- uniqueById declaredPairs
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map mkDiag unused

    isIgnored name = T.isPrefixOf "_" name

    collectDecls = concatMap declsInStmt
    declsInStmt stmt =
      case stmt of
        SLet name _ -> [name]
        SVar name _ -> [name]
        SIf _ t e -> collectDecls t <> maybe [] collectDecls e
        SWhile _ body -> collectDecls body
        SLoop body cont -> collectDecls body <> maybe [] collectDecls cont
        SFor initStmt _ contStmt body ->
          maybe [] declsInStmt initStmt <> maybe [] declsInStmt contStmt <> collectDecls body
        SSwitch _ cases defBody ->
          concatMap (collectDecls . scBody) cases <> maybe [] collectDecls defBody
        _ -> []

collectUnusedParameterDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedParameterDiagnostics diagConfig ast =
  case Map.lookup "unused_parameter" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name = Diagnostic sev "unused_parameter" ("unused parameter: " <> T.unpack name) Nothing Nothing
      in concatMap (unusedParamsInFunction mkDiag) (modFunctions ast)
          <> maybe [] (unusedParamsInEntry mkDiag) (modEntry ast)
  where
    unusedParamsInFunction mkDiag fn =
      let params = map paramName (fnParams fn)
          (nt1, paramPairs) = internNamePairs emptyNameTable params
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts (fnBody fn))
          unused =
            [ name
            | (name, ident) <- uniqueById paramPairs
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map mkDiag unused
    unusedParamsInEntry mkDiag entry =
      let params = map paramName (epParams entry)
          (nt1, paramPairs) = internNamePairs emptyNameTable params
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts (epBody entry))
          unused =
            [ name
            | (name, ident) <- uniqueById paramPairs
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map mkDiag unused
    isIgnored name = T.isPrefixOf "_" name

collectShadowingDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectShadowingDiagnostics diagConfig ast =
  case Map.lookup "shadowing" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name = Diagnostic sev "shadowing" ("name shadows outer scope: " <> T.unpack name) Nothing Nothing
      in concatMap (shadowingInFunction mkDiag) (modFunctions ast)
          <> maybe [] (shadowingInEntry mkDiag) (modEntry ast)
  where
    shadowingInFunction mkDiag fn =
      let params = map paramName (fnParams fn)
          (nt1, paramIds) = internNames emptyNameTable params
      in shadowingInStmts mkDiag nt1 [IntSet.empty, IntSet.fromList paramIds] (fnBody fn)
    shadowingInEntry mkDiag entry =
      let params = map paramName (epParams entry)
          (nt1, paramIds) = internNames emptyNameTable params
      in shadowingInStmts mkDiag nt1 [IntSet.empty, IntSet.fromList paramIds] (epBody entry)

    shadowingInStmts mkDiag table scopes0 stmts = let (diags, _, _) = go table scopes0 stmts in diags
      where
        go table0 scopesAcc [] = ([], table0, scopesAcc)
        go table0 scopesAcc (stmt:rest) =
          let (diags1, table1, scopes1) = shadowingInStmt mkDiag table0 scopesAcc stmt
              (diags2, table2, scopes2) = go table1 scopes1 rest
          in (diags1 <> diags2, table2, scopes2)

    shadowingInStmt mkDiag table scopes stmt =
      case stmt of
        SLet name _ ->
          let (ident, table') = internName name table
          in (diagIfShadow mkDiag scopes ident name, table', addToCurrent ident scopes)
        SVar name _ ->
          let (ident, table') = internName name table
          in (diagIfShadow mkDiag scopes ident name, table', addToCurrent ident scopes)
        SIf _ thenBody elseBody ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes thenBody
              (diags2, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) elseBody
          in (diags1 <> diags2, table2, scopes)
        SWhile _ body ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes body
          in (diags1, table1, scopes)
        SLoop body continuing ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes body
              (diags2, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) continuing
          in (diags1 <> diags2, table2, scopes)
        SFor initStmt _ contStmt body ->
          let loopScopes = pushScope scopes
              (diagsInit, table1, loopScopes1) =
                case initStmt of
                  Nothing -> ([], table, loopScopes)
                  Just s -> shadowingInStmt mkDiag table loopScopes s
              (diagsCont, table2, loopScopes2) =
                case contStmt of
                  Nothing -> ([], table1, loopScopes1)
                  Just s -> shadowingInStmt mkDiag table1 loopScopes1 s
              (diagsBody, table3) = shadowingInBlock mkDiag table2 loopScopes2 body
          in (diagsInit <> diagsCont <> diagsBody, table3, scopes)
        SSwitch _ cases defBody ->
          let (diagsCases, table1) = shadowingInCases mkDiag table scopes cases
              (diagsDef, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) defBody
          in (diagsCases <> diagsDef, table2, scopes)
        _ -> ([], table, scopes)

    shadowingInBlock mkDiag table scopes body = shadowingInStmtsWithTable mkDiag table (pushScope scopes) body

    shadowingInStmtsWithTable mkDiag table scopes stmts = go table scopes stmts
      where
        go table0 _ [] = ([], table0)
        go table0 scopes0 (stmt:rest) =
          let (diags1, table1, scopes1) = shadowingInStmt mkDiag table0 scopes0 stmt
              (diags2, table2) = go table1 scopes1 rest
          in (diags1 <> diags2, table2)

    shadowingInCases mkDiag table scopes cases =
      foldl' step ([], table) cases
      where
        step (diagsAcc, tableAcc) sc =
          let (diags, table') = shadowingInBlock mkDiag tableAcc scopes (scBody sc)
          in (diagsAcc <> diags, table')

    diagIfShadow mkDiag scopes ident name
      | isIgnored name = []
      | otherwise =
          let outers = drop 1 scopes
          in if any (IntSet.member ident) outers then [mkDiag name] else []

    addToCurrent ident scopes =
      case scopes of
        [] -> [IntSet.singleton ident]
        current : rest -> IntSet.insert ident current : rest

    pushScope scopes = IntSet.empty : scopes

    isIgnored name = T.isPrefixOf "_" name

collectConstantConditionDiagnostics :: DiagnosticConfig -> ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ModuleAst -> [Diagnostic]
collectConstantConditionDiagnostics diagConfig ctx constIndex fnIndex structIndex ast =
  case Map.lookup "constant_condition" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag msg = Diagnostic sev "constant_condition" msg Nothing Nothing
      in concatMap (constantCondInStmts mkDiag) (map fnBody (modFunctions ast))
          <> maybe [] (constantCondInStmts mkDiag . epBody) (modEntry ast)
  where
    constantCondInStmts mkDiag stmts = concatMap (constantCondInStmt mkDiag) stmts

    constantCondInStmt mkDiag stmt =
      case stmt of
        SIf cond thenBody elseBody ->
          checkCond mkDiag "if" cond
            <> constantCondInStmts mkDiag thenBody
            <> maybe [] (constantCondInStmts mkDiag) elseBody
        SWhile cond body ->
          checkCond mkDiag "while" cond <> constantCondInStmts mkDiag body
        SLoop body continuing ->
          constantCondInStmts mkDiag body <> maybe [] (constantCondInStmts mkDiag) continuing
        SFor initStmt condExpr contStmt body ->
          maybe [] (constantCondInStmt mkDiag) initStmt
            <> maybe [] (constantCondInStmt mkDiag) contStmt
            <> maybe [] (checkCond mkDiag "for" ) condExpr
            <> constantCondInStmts mkDiag body
        SSwitch _ cases defBody ->
          concatMap (constantCondInStmts mkDiag . scBody) cases <> maybe [] (constantCondInStmts mkDiag) defBody
        SBreakIf cond -> checkCond mkDiag "break if" cond
        _ -> []

    checkCond mkDiag label cond =
      case evalConstBoolExpr ctx constIndex fnIndex structIndex cond of
        Right _ -> [mkDiag ("constant condition in " <> label)]
        Left _ -> []

collectDuplicateCaseDiagnostics :: DiagnosticConfig -> ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ModuleAst -> [Diagnostic]
collectDuplicateCaseDiagnostics diagConfig ctx constIndex fnIndex structIndex ast =
  case Map.lookup "duplicate_case" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag val = Diagnostic sev "duplicate_case" ("duplicate switch case selector: " <> val) Nothing Nothing
      in concatMap (duplicateInStmts mkDiag) (map fnBody (modFunctions ast))
          <> maybe [] (duplicateInStmts mkDiag . epBody) (modEntry ast)
  where
    duplicateInStmts mkDiag stmts = concatMap (duplicateInStmt mkDiag) stmts

    duplicateInStmt mkDiag stmt =
      case stmt of
        SIf _ thenBody elseBody ->
          duplicateInStmts mkDiag thenBody <> maybe [] (duplicateInStmts mkDiag) elseBody
        SWhile _ body -> duplicateInStmts mkDiag body
        SLoop body continuing ->
          duplicateInStmts mkDiag body <> maybe [] (duplicateInStmts mkDiag) continuing
        SFor initStmt _ contStmt body ->
          maybe [] (duplicateInStmt mkDiag) initStmt
            <> maybe [] (duplicateInStmt mkDiag) contStmt
            <> duplicateInStmts mkDiag body
        SSwitch _ cases defBody ->
          let selectors = concatMap scSelectors cases
              values = mapMaybe (constSelectorValue ctx constIndex fnIndex structIndex) selectors
              keys = map renderKey values
              dupes = duplicateValues keys
              diags = map mkDiag dupes
              nested = concatMap (duplicateInStmts mkDiag . scBody) cases <> maybe [] (duplicateInStmts mkDiag) defBody
          in diags <> nested
        _ -> []

    constSelectorValue ctx' constIndex' fnIndex' structIndex' expr =
      case evalConstIntExpr ctx' constIndex' fnIndex' structIndex' expr of
        Right (ConstInt scalar val) -> Just (scalar, val)
        Left _ -> Nothing

    duplicateValues vals =
      let (dups, _) =
            foldl'
              (\(acc, seen) v ->
                 if Set.member v seen
                   then (Set.insert v acc, seen)
                   else (acc, Set.insert v seen))
              (Set.empty, Set.empty)
              vals
      in Set.toList dups

    renderKey (scalar, val) =
      show val <> case scalar of
        U32 -> "u"
        I32 -> "i"
        _ -> ""

collectUsesInStmts :: [Stmt] -> [Text]
collectUsesInStmts = concatMap collectUsesInStmt

collectUsesInStmt :: Stmt -> [Text]
collectUsesInStmt stmt =
  case stmt of
    SLet _ expr -> collectUsesInExpr expr
    SVar _ expr -> collectUsesInExpr expr
    SAssign lv expr -> collectUsesInLValue lv <> collectUsesInExpr expr
    SAssignOp lv _ expr -> collectUsesInLValue lv <> collectUsesInExpr expr
    SInc lv -> collectUsesInLValue lv
    SDec lv -> collectUsesInLValue lv
    SExpr expr -> collectUsesInExpr expr
    SIf cond t e -> collectUsesInExpr cond <> collectUsesInStmts t <> maybe [] collectUsesInStmts e
    SWhile cond body -> collectUsesInExpr cond <> collectUsesInStmts body
    SLoop body cont -> collectUsesInStmts body <> maybe [] collectUsesInStmts cont
    SFor initStmt condExpr contStmt body ->
      maybe [] collectUsesInStmt initStmt
        <> maybe [] collectUsesInExpr condExpr
        <> maybe [] collectUsesInStmt contStmt
        <> collectUsesInStmts body
    SSwitch expr cases defBody ->
      collectUsesInExpr expr <> concatMap collectUsesInCase cases <> maybe [] collectUsesInStmts defBody
    SBreakIf expr -> collectUsesInExpr expr
    SReturn mexpr -> maybe [] collectUsesInExpr mexpr
    _ -> []

collectUsesInCase :: SwitchCase -> [Text]
collectUsesInCase sc =
  concatMap collectUsesInExpr (scSelectors sc) <> collectUsesInStmts (scBody sc)

collectUsesInExpr :: Expr -> [Text]
collectUsesInExpr expr =
  case expr of
    EVar name -> [name]
    EInt _ -> []
    EFloat _ -> []
    EBool _ -> []
    EBinary _ a b -> collectUsesInExpr a <> collectUsesInExpr b
    EUnary _ a -> collectUsesInExpr a
    ECall _ args -> concatMap collectUsesInExpr args
    EBitcast _ arg -> collectUsesInExpr arg
    EField base _ -> collectUsesInExpr base
    EIndex base idx -> collectUsesInExpr base <> collectUsesInExpr idx

collectUsesInLValue :: LValue -> [Text]
collectUsesInLValue lv =
  case lv of
    LVVar name -> [name]
    LVField base _ -> collectUsesInLValue base
    LVIndex base idx -> collectUsesInLValue base <> collectUsesInExpr idx
    LVDeref expr -> collectUsesInExpr expr

validateStruct :: ModuleContext -> Scope -> StructDecl -> Either CompileError ()
validateStruct ctx scope decl =
  mapM_ (validateType ctx scope . fdType) (sdFields decl)

validateBinding :: ModuleContext -> Scope -> BindingDecl -> Either CompileError ()
validateBinding ctx scope decl = validateType ctx scope (bdType decl)

validateGlobalVar :: ModuleContext -> Scope -> GlobalVarDecl -> Either CompileError ()
validateGlobalVar ctx scope decl = do
  validateType ctx scope (gvType decl)
  mapM_ (validateExpr ctx scope) (gvInit decl)
  case gvType decl of
    TyPtr {} -> Left (CompileError "global pointer types are not supported" Nothing Nothing)
    _ -> Right ()
  case gvSpace decl of
    "private" -> Right ()
    "workgroup" ->
      case gvInit decl of
        Nothing -> Right ()
        Just _ -> Left (CompileError "workgroup variables cannot have initializers" Nothing Nothing)
    _ -> Left (CompileError ("unsupported global address space: " <> textToString (gvSpace decl)) Nothing Nothing)

validateConst :: ModuleContext -> Scope -> ConstDecl -> Either CompileError ()
validateConst ctx scope decl = validateExpr ctx scope (cdExpr decl) >> Right ()

validateAlias :: ModuleContext -> Scope -> AliasDecl -> Either CompileError ()
validateAlias ctx scope decl = validateType ctx scope (adType decl)

validateOverride :: ModuleContext -> Scope -> OverrideIndex -> OverrideDecl -> Either CompileError ()
validateOverride ctx scope _overrideIndex decl = do
  validateType ctx scope (odType decl)
  case odExpr decl of
    Nothing -> Right ()
    Just expr -> do
      validateExpr ctx scope expr
      Right ()
  where
    _ = _overrideIndex

validateConstAssert :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> DiagnosticConfig -> ConstAssert -> Either CompileError ()
validateConstAssert ctx constIndex fnIndex structIndex diagConfig (ConstAssert pos expr) =
  case diagnosticSeverity diagConfig "const_assert" of
    DiagOff -> Right ()
    DiagInfo -> Right ()
    DiagWarning -> Right ()
    DiagError -> do
      ok <- withPos pos (evalConstBoolExpr ctx constIndex fnIndex structIndex expr)
      if ok
        then Right ()
        else Left (errorAtPos pos "const_assert condition failed")

validateFunction :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> FunctionDecl -> Either CompileError ()
validateFunction ctx constIndex fnIndex structIndex skipConstEval scope fn = do
  mapM_ (validateType ctx scope . paramType) (fnParams fn)
  mapM_ (validateType ctx scope) (maybeToList (fnReturnType fn))
  let paramNames = map paramName (fnParams fn)
  ensureNoDuplicates "function parameters" paramNames
  let scope1 = scopeWithParams scope paramNames
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope1 (fnBody fn)

validateEntryPoint :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> EntryPoint -> Either CompileError ()
validateEntryPoint ctx constIndex fnIndex structIndex skipConstEval scope entry = do
  mapM_ (validateType ctx scope . paramType) (epParams entry)
  mapM_ (validateType ctx scope) (maybeToList (epReturnType entry))
  let paramNames = map paramName (epParams entry)
  ensureNoDuplicates "entry point parameters" paramNames
  let scope1 = scopeWithParams scope paramNames
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope1 (epBody entry)

validateStmtList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> [Stmt] -> Either CompileError ()
validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope0 = go scope0
  where
    go _ [] = Right ()
    go sc (stmt:rest) = do
      sc' <- validateStmt ctx constIndex fnIndex structIndex skipConstEval sc stmt
      go sc' rest

validateStmt :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> Stmt -> Either CompileError Scope
validateStmt ctx constIndex fnIndex structIndex skipConstEval scope stmt =
  case stmt of
    SLet name expr -> do
      validateExpr ctx scope expr
      scopeAdd scope name
    SVar name expr -> do
      validateExpr ctx scope expr
      scopeAdd scope name
    SAssign lv expr -> validateLValue ctx scope lv >> validateExpr ctx scope expr >> Right scope
    SAssignOp lv _ expr -> validateLValue ctx scope lv >> validateExpr ctx scope expr >> Right scope
    SInc lv -> validateLValue ctx scope lv >> Right scope
    SDec lv -> validateLValue ctx scope lv >> Right scope
    SExpr expr -> validateExpr ctx scope expr >> Right scope
    SIf cond thenBody elseBody -> do
      validateExpr ctx scope cond
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) thenBody
      mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) elseBody
      Right scope
    SWhile cond body -> do
      validateExpr ctx scope cond
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) body
      Right scope
    SLoop body continuing -> do
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) body
      mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) continuing
      Right scope
    SFor initStmt condExpr contStmt body -> do
      scope1 <- case initStmt of
        Nothing -> Right scope
        Just s -> validateStmt ctx constIndex fnIndex structIndex skipConstEval scope s
      mapM_ (validateExpr ctx scope1) condExpr
      mapM_ (validateStmt ctx constIndex fnIndex structIndex skipConstEval scope1) contStmt
      validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope1) body
      Right scope
    SSwitch expr cases defBody -> do
      validateExpr ctx scope expr
      mapM_ (validateSwitchCase ctx constIndex fnIndex structIndex skipConstEval scope) cases
      mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) defBody
      Right scope
    SBreak -> Right scope
    SBreakIf cond -> validateExpr ctx scope cond >> Right scope
    SContinue -> Right scope
    SDiscard -> Right scope
    SFallthrough ->
      if scAllowFallthrough scope
        then Right scope
        else Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
    SReturn mexpr -> mapM_ (validateExpr ctx scope) mexpr >> Right scope

validateSwitchCase :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> SwitchCase -> Either CompileError ()
validateSwitchCase ctx constIndex fnIndex structIndex skipConstEval scope sc = do
  _ <- analyzeFallthroughPlacement (scBody sc)
  when (not skipConstEval) $
    mapM_ (evalConstIntExpr ctx constIndex fnIndex structIndex) (scSelectors sc)
  let scope' = scope { scAllowFallthrough = True }
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope') (scBody sc)

analyzeFallthroughPlacement :: [Stmt] -> Either CompileError (Bool, [Stmt])
analyzeFallthroughPlacement body =
  case body of
    [] -> Right (False, [])
    _ ->
      let initStmts = init body
          lastStmt = last body
          nestedFallthrough = any stmtHasFallthrough initStmts
      in case lastStmt of
          SFallthrough ->
            if nestedFallthrough
              then Left (CompileError "fallthrough must be the last statement in a switch case" Nothing Nothing)
              else Right (True, initStmts)
          _ ->
            if nestedFallthrough || stmtHasFallthrough lastStmt
              then Left (CompileError "fallthrough must be the last statement in a switch case" Nothing Nothing)
              else Right (False, body)

stmtHasFallthrough :: Stmt -> Bool
stmtHasFallthrough stmt =
  case stmt of
    SFallthrough -> True
    SIf _ thenBody elseBody ->
      any stmtHasFallthrough thenBody || maybe False (any stmtHasFallthrough) elseBody
    SWhile _ body -> any stmtHasFallthrough body
    SLoop body continuing ->
      any stmtHasFallthrough body || maybe False (any stmtHasFallthrough) continuing
    SFor initStmt _ condStmt body ->
      maybe False stmtHasFallthrough initStmt
        || maybe False stmtHasFallthrough condStmt
        || any stmtHasFallthrough body
    SSwitch _ _ _ -> False
    _ -> False

expandSwitchCases :: [SwitchCase] -> Maybe [Stmt] -> Either CompileError [SwitchCase]
expandSwitchCases cases defBody = do
  infos <- mapM extract cases
  go [] defBody (reverse infos)
  where
    extract (SwitchCase selectors body) = do
      (fallthrough, stripped) <- analyzeFallthroughPlacement body
      Right (selectors, stripped, fallthrough)

    go acc _ [] = Right acc
    go acc nextChain ((selectors, body, fallthrough):rest) =
      case fallthrough of
        True ->
          case nextChain of
            Nothing ->
              Left (CompileError "fallthrough requires a following case or default" Nothing Nothing)
            Just chain ->
              let combined = body <> chain
              in go (SwitchCase selectors combined : acc) (Just combined) rest
        False ->
          let combined = body
          in go (SwitchCase selectors combined : acc) (Just combined) rest

data ConstInt = ConstInt
  { ciScalar :: Scalar
  , ciValue :: Integer
  } deriving (Eq, Show)

data ConstFloat = ConstFloat
  { cfScalar :: Scalar
  , cfValue :: Double
  } deriving (Eq, Show)

data ConstValue
  = CVInt ConstInt
  | CVBool Bool
  | CVFloat ConstFloat
  | CVVector Int Scalar [ConstValue]
  | CVMatrix Int Int Scalar [ConstValue]
  | CVArray Type [ConstValue]
  | CVStruct Text [(Text, ConstValue)]
  | CVPointer Type LValue
  deriving (Eq, Show)

data ConstBinding = ConstBinding
  { cbValue :: ConstValue
  , cbMutable :: Bool
  } deriving (Eq, Show)

type ConstEnv = Map.Map Text ConstBinding

constValueType :: ConstValue -> Type
constValueType val =
  case val of
    CVInt (ConstInt scalar _) -> TyScalar scalar
    CVFloat (ConstFloat scalar _) -> TyScalar scalar
    CVBool _ -> TyScalar Bool
    CVVector n scalar _ -> TyVector n scalar
    CVMatrix cols rows scalar _ -> TyMatrix cols rows scalar
    CVArray elemTy vals -> TyArray elemTy (Just (length vals))
    CVStruct name _ -> TyStructRef name
    CVPointer ty _ -> ty

constScalarType :: ConstValue -> Either CompileError Scalar
constScalarType val =
  case val of
    CVInt (ConstInt scalar _) -> Right scalar
    CVFloat (ConstFloat scalar _) -> Right scalar
    CVBool _ -> Right Bool
    _ -> Left (CompileError "expected scalar constant" Nothing Nothing)

constValueToInt :: ConstValue -> Either CompileError ConstInt
constValueToInt val =
  case val of
    CVInt v -> Right v
    _ -> Left (CompileError "expected integer constant" Nothing Nothing)

constValueToFloat :: ConstValue -> Either CompileError ConstFloat
constValueToFloat val =
  case val of
    CVFloat v -> Right v
    CVInt (ConstInt _ v) -> Right (ConstFloat F32 (fromIntegral v))
    _ -> Left (CompileError "expected float constant" Nothing Nothing)

constValueToBool :: ConstValue -> Either CompileError Bool
constValueToBool val =
  case val of
    CVBool b -> Right b
    _ -> Left (CompileError "expected bool constant" Nothing Nothing)

coerceConstScalarValue :: Scalar -> ConstValue -> Either CompileError ConstValue
coerceConstScalarValue target val =
  case target of
    Bool ->
      case val of
        CVBool b -> Right (CVBool b)
        _ -> Left (CompileError "expected bool constant" Nothing Nothing)
    I32 ->
      case val of
        CVInt ci -> CVInt <$> coerceConstIntToScalar I32 ci
        CVFloat (ConstFloat _ v) -> do
          let n = truncate v :: Integer
          when (n < 0 || n > 0x7FFFFFFF) $
            Left (CompileError "constant i32 is out of range" Nothing Nothing)
          Right (CVInt (ConstInt I32 n))
        _ -> Left (CompileError "expected integer constant" Nothing Nothing)
    U32 ->
      case val of
        CVInt ci -> CVInt <$> coerceConstIntToScalar U32 ci
        CVFloat (ConstFloat _ v) -> do
          let n = truncate v :: Integer
          when (n < 0 || n > fromIntegral (maxBound :: Word32)) $
            Left (CompileError "constant u32 is out of range" Nothing Nothing)
          Right (CVInt (ConstInt U32 n))
        _ -> Left (CompileError "expected integer constant" Nothing Nothing)
    F32 ->
      case val of
        CVFloat cf -> Right (CVFloat (convertConstFloatTo F32 cf))
        CVInt (ConstInt _ v) -> Right (CVFloat (ConstFloat F32 (fromIntegral v)))
        _ -> Left (CompileError "expected float constant" Nothing Nothing)
    F16 ->
      case val of
        CVFloat cf -> Right (CVFloat (convertConstFloatTo F16 cf))
        CVInt (ConstInt _ v) -> Right (CVFloat (convertConstFloatTo F16 (ConstFloat F32 (fromIntegral v))))
        _ -> Left (CompileError "expected float constant" Nothing Nothing)

coerceConstValueToType :: ModuleContext -> StructIndex -> Type -> ConstValue -> Either CompileError ConstValue
coerceConstValueToType ctx structIndex target val =
  case target of
    TyScalar scalar -> coerceConstScalarValue scalar val
    TyVector n scalar ->
      case val of
        CVVector m _ comps | m == n -> do
          comps' <- mapM (coerceConstScalarValue scalar) comps
          Right (CVVector n scalar comps')
        _ -> Left (CompileError "vector constant does not match type" Nothing Nothing)
    TyMatrix cols rows scalar ->
      case val of
        CVMatrix c r _ colsVals | c == cols && r == rows -> do
          cols' <- mapM (coerceConstValueToType ctx structIndex (TyVector rows scalar)) colsVals
          Right (CVMatrix cols rows scalar cols')
        _ -> Left (CompileError "matrix constant does not match type" Nothing Nothing)
    TyArray elemTy (Just len) ->
      case val of
        CVArray _ elems | length elems == len -> do
          elems' <- mapM (coerceConstValueToType ctx structIndex elemTy) elems
          Right (CVArray elemTy elems')
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TyArray _ Nothing ->
      Left (CompileError "runtime array constants are not supported" Nothing Nothing)
    TyStructRef name ->
      case val of
        CVStruct structName pairs | structName == name -> do
          decl <- resolveStructDecl ctx structIndex name
          fields' <- mapM (coerceField pairs) (sdFields decl)
          Right (CVStruct name fields')
        _ -> Left (CompileError "struct constant does not match type" Nothing Nothing)
    TyPtr space access elemTy ->
      case val of
        CVPointer ptrTy lv ->
          case ptrTy of
            TyPtr space' access' elemTy' ->
              if space == space' && elemTy == elemTy' && ptrAccessCompatible access access'
                then Right (CVPointer ptrTy lv)
                else Left (CompileError "pointer constant does not match type" Nothing Nothing)
            _ -> Left (CompileError "pointer constant does not match type" Nothing Nothing)
        _ -> Left (CompileError "pointer constant does not match type" Nothing Nothing)
    _ -> Left (CompileError "unsupported const type" Nothing Nothing)
  where
    coerceField pairs field = do
      val' <- case lookup (fdName field) pairs of
        Just v -> Right v
        Nothing -> Left (CompileError ("missing field: " <> textToString (fdName field)) Nothing Nothing)
      coerced <- coerceConstValueToType ctx structIndex (fdType field) val'
      Right (fdName field, coerced)

resolveStructDecl :: ModuleContext -> StructIndex -> Text -> Either CompileError StructDecl
resolveStructDecl ctx structIndex name =
  case splitQName name of
    [] -> Left (CompileError "invalid struct reference" Nothing Nothing)
    [single] ->
      case lookupStruct (mcPath ctx) single of
        Just decl -> Right decl
        Nothing ->
          case Map.lookup single (mcItemAliases ctx) of
            Just target ->
              case splitLast target of
                Nothing -> Left (CompileError "invalid struct reference" Nothing Nothing)
                Just (path, item) ->
                  case lookupStruct path item of
                    Just decl -> Right decl
                    Nothing -> Left (CompileError ("unknown struct: " <> textToString item) Nothing Nothing)
            Nothing -> Left (CompileError ("unknown struct: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid struct reference" Nothing Nothing)
            Just (path, item) ->
              case lookupStruct path item of
                Just decl -> Right decl
                Nothing -> Left (CompileError ("unknown struct: " <> textToString item) Nothing Nothing)
  where
    lookupStruct path item = lookupStructIndex structIndex path item

evalConstValueWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstValue
evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EUnary OpAddr inner -> evalConstAddressOf seen fnSeen inner
        EUnary OpDeref inner -> do
          ptr <- go seen fnSeen inner
          derefConstPointer seen fnSeen ptr
        EVar name ->
          case Map.lookup name env of
            Just envBinding -> Right (cbValue envBinding)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating constant expression" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstValueWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField base field -> do
          baseVal <- go seen fnSeen base
          evalConstFieldAccess baseVal field
        EIndex base idxExpr -> do
          baseVal <- go seen fnSeen base
          idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen idxExpr
          evalConstIndexAccess baseVal idxVal
        ECall name args ->
          case name of
            "vec2" -> evalConstVectorCtor 2 seen fnSeen args
            "vec3" -> evalConstVectorCtor 3 seen fnSeen args
            "vec4" -> evalConstVectorCtor 4 seen fnSeen args
            "array" -> evalConstArrayCtor seen fnSeen args
            _ | Just (cols, rows) <- parseMatrixName name ->
                  evalConstMatrixCtor cols rows seen fnSeen args
            _ ->
              if isBuiltinName name
                then evalConstScalarValue seen fnSeen ex
                else
                  case resolveStructDecl ctx structIndex name of
                    Right decl -> evalConstStructCtor (sdName decl) (sdFields decl) seen fnSeen args
                    Left _ ->
                      evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
        _ -> evalConstScalarValue seen fnSeen ex

    evalConstScalarValue seen fnSeen ex =
      case evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex of
        Right v -> Right (CVInt v)
        Left errI ->
          case evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex of
            Right v -> Right (CVFloat v)
            Left errF ->
              case evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex of
                Right v -> Right (CVBool v)
                Left _ -> Left (firstError errI errF)

    firstError errI _ = errI

    evalConstVectorCtor n seen fnSeen args = do
      when (length args /= n) $
        Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      case vals of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        (firstVal : _) -> do
          scalar <- constScalarType firstVal
          coerced <- mapM (coerceConstScalarValue scalar) vals
          Right (CVVector n scalar coerced)

    evalConstMatrixCtor cols rows seen fnSeen args = do
      when (null args) $
        Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      let scalarCount = cols * rows
      if length vals == scalarCount && all isScalarConst vals
        then
          case vals of
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
            (v:_) -> do
              scalar <- constScalarType v
              scalars <- mapM (coerceConstScalarValue scalar) vals
              let colsVals = map (CVVector rows scalar) (chunk rows scalars)
              Right (CVMatrix cols rows scalar colsVals)
        else do
          when (length vals /= cols) $
            Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
          case vals of
            (firstCol : _) -> do
              scalar <- case firstCol of
                CVVector n scalar _ | n == rows -> Right scalar
                _ -> Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
              mapM_ (ensureColumn scalar) vals
              Right (CVMatrix cols rows scalar vals)
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
      where
        isScalarConst v =
          case v of
            CVInt _ -> True
            CVFloat _ -> True
            CVBool _ -> True
            _ -> False
        ensureColumn scalar val =
          case val of
            CVVector n s _ | n == rows && s == scalar -> Right ()
            CVVector _ _ _ -> Left (CompileError "matrix constructor expects column vectors" Nothing Nothing)
            _ -> Left (CompileError "matrix constructor expects column vectors" Nothing Nothing)
        chunk n xs =
          case splitAt n xs of
            (col, []) | null col -> []
            (col, rest) -> col : chunk n rest

    evalConstArrayCtor seen fnSeen args = do
      when (null args) $
        Left (CompileError "array constructor needs arguments" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      case vals of
        [] -> Left (CompileError "array constructor needs arguments" Nothing Nothing)
        (v:vs) ->
          case v of
            CVInt (ConstInt scalar _) -> do
              coerced <- mapM (coerceConstScalarValue scalar) (v:vs)
              Right (CVArray (TyScalar scalar) coerced)
            CVFloat (ConstFloat scalar _) -> do
              coerced <- mapM (coerceConstScalarValue scalar) (v:vs)
              Right (CVArray (TyScalar scalar) coerced)
            CVBool _ -> do
              coerced <- mapM (coerceConstScalarValue Bool) (v:vs)
              Right (CVArray (TyScalar Bool) coerced)
            _ -> do
              let elemTy = constValueType v
              mapM_ (ensureSameType elemTy) vs
              Right (CVArray elemTy (v:vs))
      where
        ensureSameType ty val =
          if constValueType val == ty
            then Right ()
            else Left (CompileError "array constructor argument type mismatch" Nothing Nothing)

    evalConstStructCtor name fields seen fnSeen args = do
      when (length args /= length fields) $
        Left (CompileError ("struct constructor arity mismatch for " <> textToString name) Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      let pairs = zip (map fdName fields) vals
      Right (CVStruct name pairs)

    evalConstAddressOf seen fnSeen inner =
      case exprToLValue inner of
        Nothing -> Left (CompileError "address-of requires an lvalue" Nothing Nothing)
        Just lv ->
          case lv of
            LVDeref ptrExpr -> do
              ptr <- go seen fnSeen ptrExpr
              case ptr of
                CVPointer _ _ -> Right ptr
                _ -> Left (CompileError "address-of expects a pointer operand" Nothing Nothing)
            _ -> do
              frozen <- freezeConstLValue seen fnSeen lv
              val <- evalConstLValueGet ctx constIndex fnIndex structIndex env seen fnSeen frozen
              let ptrTy = TyPtr "function" Nothing (constValueType val)
              Right (CVPointer ptrTy frozen)

    derefConstPointer seen fnSeen val =
      case val of
        CVPointer _ lv -> evalConstLValueGet ctx constIndex fnIndex structIndex env seen fnSeen lv
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

    freezeConstLValue seen fnSeen lv =
      case lv of
        LVVar name -> Right (LVVar name)
        LVField base field -> LVField <$> freezeConstLValue seen fnSeen base <*> pure field
        LVIndex base idxExpr -> do
          base' <- freezeConstLValue seen fnSeen base
          ConstInt _ idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen idxExpr
          Right (LVIndex base' (EInt idxVal))
        LVDeref _ ->
          Left (CompileError "address-of requires an lvalue" Nothing Nothing)

evalConstFunctionValueWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> FunctionDecl -> Either CompileError ConstValue
evalConstFunctionValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns decl = do
  (_, ctrl, _) <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns constEvalMaxSteps (fnBody decl)
  val <- case ctrl of
    CCReturn v -> Right v
    CCNone -> Left (CompileError "const function must return a value" Nothing Nothing)
    CCBreak -> Left (CompileError "break used outside of a loop" Nothing Nothing)
    CCContinue -> Left (CompileError "continue used outside of a loop" Nothing Nothing)
  case fnReturnType decl of
    Just ty -> coerceConstValueToType ctx structIndex ty val
    Nothing -> Left (CompileError "const function must declare a return type" Nothing Nothing)

evalConstUserFunctionCall :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Text -> [Expr] -> Either CompileError ConstValue
evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seenConsts seenFns name args = do
  (path, ident) <- resolveFunctionRef ctx name
  let key = T.intercalate "::" (path <> [ident])
  when (Set.member key seenFns) $
    Left (CompileError "cycle detected while evaluating const function" Nothing Nothing)
  decls <- case lookupFunctionIndex fnIndex path ident of
    Nothing -> Left (CompileError ("unknown function: " <> textToString ident) Nothing Nothing)
    Just ds -> Right ds
  argVals <- mapM (evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns) args
  let seenFns' = Set.insert key seenFns
  let attempt decl = do
        let params = fnParams decl
        when (length params /= length argVals) $
          Left (CompileError "const function overload not found" Nothing Nothing)
        coercedArgs <- zipWithM (coerceConstValueToType ctx structIndex . paramType) params argVals
        let bindings = [ConstBinding val False | val <- coercedArgs]
        let env' = Map.fromList (zip (map paramName params) bindings)
        evalConstFunctionValueWithEnv ctx constIndex fnIndex structIndex env' seenConsts seenFns' decl
  let (errs, results) = partitionEithers (map attempt decls)
  case results of
    [v] -> Right v
    [] ->
      case errs of
        (err:_) -> Left err
        [] -> Left (CompileError "const function overload not found" Nothing Nothing)
    _ -> Left (CompileError "const function overload is ambiguous" Nothing Nothing)

data ConstControl
  = CCNone
  | CCReturn ConstValue
  | CCBreak
  | CCContinue
  deriving (Eq, Show)

constEvalMaxSteps :: Int
constEvalMaxSteps = 20000

evalConstStmtList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Int -> [Stmt] -> Either CompileError (ConstEnv, ConstControl, Int)
evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel stmts = go env fuel stmts
  where
    go env' fuel' [] = Right (env', CCNone, fuel')
    go env' fuel' (stmt:rest) = do
      (envNext, ctrl, fuelNext) <- evalConstStmt ctx constIndex fnIndex structIndex env' seenConsts seenFns fuel' stmt
      case ctrl of
        CCNone -> go envNext fuelNext rest
        _ -> Right (envNext, ctrl, fuelNext)

evalConstStmt :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Int -> Stmt -> Either CompileError (ConstEnv, ConstControl, Int)
evalConstStmt ctx constIndex fnIndex structIndex env seenConsts seenFns fuel stmt = do
  fuel' <- consumeConstFuel fuel
  case stmt of
    SLet name expr -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (Map.insert name (ConstBinding val False) env, CCNone, fuel')
    SVar name expr -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (Map.insert name (ConstBinding val True) env, CCNone, fuel')
    SAssign lv expr -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      newVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      let targetTy = constValueType oldVal
      newVal' <- coerceConstValueToType ctx structIndex targetTy newVal
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal'
      Right (env', CCNone, fuel')
    SAssignOp lv op expr -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      rhsVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      let targetTy = constValueType oldVal
      rhsVal' <- coerceConstValueToType ctx structIndex targetTy rhsVal
      newVal <- evalConstAssignOp op oldVal rhsVal'
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
      Right (env', CCNone, fuel')
    SInc lv -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      one <- constScalarOne oldVal
      newVal <- evalConstAssignOp OpAdd oldVal one
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
      Right (env', CCNone, fuel')
    SDec lv -> do
      oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
      one <- constScalarOne oldVal
      newVal <- evalConstAssignOp OpSub oldVal one
      env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
      Right (env', CCNone, fuel')
    SExpr expr -> do
      _ <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (env, CCNone, fuel')
    SSwitch expr cases defBody -> do
      cases' <- expandSwitchCases cases defBody
      ConstInt _ selector <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      matchCase selector cases' defBody fuel'
    SIf cond thenBody elseBody -> do
      ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns cond
      if ok
        then do
          (env', ctrl, fuel'') <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel' thenBody
          Right (env', ctrl, fuel'')
        else
          case elseBody of
            Nothing -> Right (env, CCNone, fuel')
            Just body -> do
              (env', ctrl, fuel'') <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel' body
              Right (env', ctrl, fuel'')
    SWhile cond body -> evalConstWhile cond body fuel'
    SLoop body continuing -> evalConstLoop body continuing fuel'
    SFor initStmt condExpr contStmt body -> evalConstFor initStmt condExpr contStmt body fuel'
    SBreak -> Right (env, CCBreak, fuel')
    SBreakIf cond -> do
      ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns cond
      Right (env, if ok then CCBreak else CCNone, fuel')
    SContinue -> Right (env, CCContinue, fuel')
    SReturn (Just expr) -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      Right (env, CCReturn val, fuel')
    SReturn Nothing ->
      Left (CompileError "const function return requires a value" Nothing Nothing)
    _ -> Left (CompileError "const function bodies may only contain let/var, if, switch, loops, assignments, expr, and return statements" Nothing Nothing)
  where
    matchCase _ [] defBody fuel'' =
      case defBody of
        Nothing -> Right (env, CCNone, fuel'')
        Just body -> evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' body
    matchCase selector (case0:rest) defBody fuel'' = do
      matched <- or <$> mapM (matchesSelector selector) (scSelectors case0)
      if matched
        then evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' (scBody case0)
        else matchCase selector rest defBody fuel''

    matchesSelector selector ex = do
      ConstInt _ val <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns ex
      Right (val == selector)

    consumeConstFuel n =
      if n <= 0
        then Left (CompileError "const evaluation exceeded step limit" Nothing Nothing)
        else Right (n - 1)

    evalConstWhile cond body fuel'' = loop fuel'' env
      where
        loop fuelLoop envLoop = do
          fuelNext <- consumeConstFuel fuelLoop
          ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex envLoop seenConsts seenFns cond
          if not ok
            then Right (envLoop, CCNone, fuelNext)
            else do
              (envBody, ctrl, fuelBody) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelNext body
              case ctrl of
                CCNone -> loop fuelBody envBody
                CCContinue -> loop fuelBody envBody
                CCBreak -> Right (envBody, CCNone, fuelBody)
                _ -> Right (envBody, ctrl, fuelBody)

    evalConstLoop body continuing fuel'' = loop fuel'' env
      where
        loop fuelLoop envLoop = do
          fuelNext <- consumeConstFuel fuelLoop
          (envBody, ctrl, fuelBody) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelNext body
          case ctrl of
            CCReturn _ -> Right (envBody, ctrl, fuelBody)
            CCBreak -> Right (envBody, CCNone, fuelBody)
            CCContinue -> runContinuing envBody fuelBody
            CCNone -> runContinuing envBody fuelBody

        runContinuing envLoop fuelLoop =
          case continuing of
            Nothing -> loop fuelLoop envLoop
            Just contBody -> do
              (envCont, ctrlCont, fuelCont) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelLoop contBody
              case ctrlCont of
                CCReturn _ -> Right (envCont, ctrlCont, fuelCont)
                CCBreak -> Right (envCont, CCNone, fuelCont)
                _ -> loop fuelCont envCont

    evalConstFor initStmt condExpr contStmt body fuel'' = do
      (envInit, ctrlInit, fuelInit) <- case initStmt of
        Nothing -> Right (env, CCNone, fuel'')
        Just s -> evalConstStmt ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' s
      case ctrlInit of
        CCNone -> loop fuelInit envInit
        _ -> Left (CompileError "invalid control flow in for initializer" Nothing Nothing)
      where
        loop fuelLoop envLoop = do
          fuelNext <- consumeConstFuel fuelLoop
          ok <- case condExpr of
            Nothing -> Right True
            Just cond -> evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex envLoop seenConsts seenFns cond
          if not ok
            then Right (envLoop, CCNone, fuelNext)
            else do
              (envBody, ctrlBody, fuelBody) <- evalConstStmtList ctx constIndex fnIndex structIndex envLoop seenConsts seenFns fuelNext body
              case ctrlBody of
                CCReturn _ -> Right (envBody, ctrlBody, fuelBody)
                CCBreak -> Right (envBody, CCNone, fuelBody)
                _ -> do
                  (envCont, ctrlCont, fuelCont) <- case contStmt of
                    Nothing -> Right (envBody, CCNone, fuelBody)
                    Just s -> evalConstStmt ctx constIndex fnIndex structIndex envBody seenConsts seenFns fuelBody s
                  case ctrlCont of
                    CCNone -> loop fuelCont envCont
                    CCContinue -> loop fuelCont envCont
                    _ -> Right (envCont, ctrlCont, fuelCont)

evalConstLValueGet :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> LValue -> Either CompileError ConstValue
evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv =
  case lv of
    LVVar name ->
      case Map.lookup name env of
        Just envBinding -> Right (cbValue envBinding)
        Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
    LVField base field -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      evalConstFieldAccess baseVal field
    LVIndex base idxExpr -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns idxExpr
      evalConstIndexAccess baseVal idxVal
    LVDeref expr -> do
      ptr <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      case ptr of
        CVPointer _ ptrLv -> evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns ptrLv
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

evalConstLValueSet :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> LValue -> ConstValue -> Either CompileError ConstEnv
evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal =
  case lv of
    LVVar name ->
        case Map.lookup name env of
          Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
          Just envBinding ->
            if cbMutable envBinding
              then Right (Map.insert name envBinding { cbValue = newVal } env)
              else Left (CompileError "cannot assign to immutable let binding" Nothing Nothing)
    LVField base field -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      updated <- updateFieldValue baseVal field newVal
      evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns base updated
    LVIndex base idxExpr -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns idxExpr
      updated <- updateIndexValue baseVal idxVal newVal
      evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns base updated
    LVDeref expr -> do
      ptr <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      case ptr of
        CVPointer _ ptrLv -> evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns ptrLv newVal
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

updateFieldValue :: ConstValue -> Text -> ConstValue -> Either CompileError ConstValue
updateFieldValue base field newVal =
  case base of
    CVStruct name pairs ->
      if any ((== field) . fst) pairs
        then Right (CVStruct name (map upd pairs))
        else Left (CompileError ("unknown field: " <> textToString field) Nothing Nothing)
      where
        upd (fname, fval)
          | fname == field = (fname, newVal)
          | otherwise = (fname, fval)
    CVVector n scalar comps -> do
      ix <- vectorFieldIndex field n
      newScalar <- coerceConstScalarValue scalar newVal
      let comps' = [if i == ix then newScalar else c | (i, c) <- zip [0..] comps]
      Right (CVVector n scalar comps')
    _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)

updateIndexValue :: ConstValue -> ConstInt -> ConstValue -> Either CompileError ConstValue
updateIndexValue base (ConstInt _ raw) newVal = do
  when (raw < 0) $
    Left (CompileError "index must be non-negative" Nothing Nothing)
  when (raw > fromIntegral (maxBound :: Int)) $
    Left (CompileError "index is out of range" Nothing Nothing)
  let ix = fromIntegral raw :: Int
  case base of
    CVVector n scalar comps ->
      if ix < n
        then do
          newScalar <- coerceConstScalarValue scalar newVal
          let comps' = [if i == ix then newScalar else c | (i, c) <- zip [0..] comps]
          Right (CVVector n scalar comps')
        else Left (CompileError "vector index out of range" Nothing Nothing)
    CVMatrix cols rows scalar colsVals ->
      if ix < cols
        then do
          let targetTy = TyVector rows scalar
          when (constValueType newVal /= targetTy) $
            Left (CompileError "matrix index assignment type mismatch" Nothing Nothing)
          let cols' = [if i == ix then newVal else c | (i, c) <- zip [0..] colsVals]
          Right (CVMatrix cols rows scalar cols')
        else Left (CompileError "matrix index out of range" Nothing Nothing)
    CVArray elemTy vals ->
      if ix < length vals
        then do
          when (constValueType newVal /= elemTy) $
            Left (CompileError "array index assignment type mismatch" Nothing Nothing)
          let vals' = [if i == ix then newVal else c | (i, c) <- zip [0..] vals]
          Right (CVArray elemTy vals')
        else Left (CompileError "array index out of range" Nothing Nothing)
    _ -> Left (CompileError "indexing requires array, vector, or matrix type" Nothing Nothing)

constScalarOne :: ConstValue -> Either CompileError ConstValue
constScalarOne val =
  case val of
    CVInt (ConstInt scalar _) -> Right (CVInt (ConstInt scalar 1))
    CVFloat (ConstFloat scalar _) -> Right (CVFloat (ConstFloat scalar 1.0))
    _ -> Left (CompileError "increment/decrement requires a scalar integer or float" Nothing Nothing)

evalConstAssignOp :: BinOp -> ConstValue -> ConstValue -> Either CompileError ConstValue
evalConstAssignOp op lhs rhs =
  case (lhs, rhs) of
    (CVInt (ConstInt scalar a), CVInt (ConstInt _ b)) -> do
      let result = applyIntOp scalar a b
      CVInt <$> result
    (CVFloat (ConstFloat scalar a), CVFloat (ConstFloat _ b)) -> do
      v <- applyFloatOp a b
      Right (CVFloat (ConstFloat scalar (if scalar == F16 then quantizeF16 v else v)))
    (CVBool a, CVBool b) ->
      case op of
        OpAnd -> Right (CVBool (a && b))
        OpOr -> Right (CVBool (a || b))
        _ -> Left (CompileError "unsupported boolean assignment operation" Nothing Nothing)
    (CVVector n scalar xs, CVVector m _ ys)
      | n == m && length xs == length ys ->
          CVVector n scalar <$> zipWithM (evalConstAssignOp op) xs ys
    (CVMatrix c r s xs, CVMatrix c' r' _ ys)
      | c == c' && r == r' && length xs == length ys ->
          CVMatrix c r s <$> zipWithM (evalConstAssignOp op) xs ys
    _ -> Left (CompileError "unsupported assignment operand types" Nothing Nothing)
  where
    applyIntOp scalar a b =
      case op of
        OpAdd -> makeInt scalar (a + b)
        OpSub -> makeInt scalar (a - b)
        OpMul -> makeInt scalar (a * b)
        OpDiv ->
          if b == 0
            then Left (CompileError "division by zero in constant expression" Nothing Nothing)
            else makeInt scalar (a `quot` b)
        OpMod ->
          if b == 0
            then Left (CompileError "modulo by zero in constant expression" Nothing Nothing)
            else makeInt scalar (a `rem` b)
        OpBitAnd -> makeInt scalar (a .&. b)
        OpBitOr -> makeInt scalar (a .|. b)
        OpBitXor -> makeInt scalar (xor a b)
        OpShl ->
          if b < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else makeInt scalar (shiftL a (fromIntegral b))
        OpShr ->
          if b < 0 then Left (CompileError "shift amount must be non-negative" Nothing Nothing) else makeInt scalar (shiftR a (fromIntegral b))
        _ -> Left (CompileError "unsupported integer assignment operation" Nothing Nothing)

    makeInt scalar n =
      case scalar of
        I32 -> checkI32 n >> Right (ConstInt I32 n)
        U32 -> checkU32 n >> Right (ConstInt U32 n)
        _ -> Left (CompileError "unsupported integer assignment operation" Nothing Nothing)

    applyFloatOp a b =
      case op of
        OpAdd -> Right (a + b)
        OpSub -> Right (a - b)
        OpMul -> Right (a * b)
        OpDiv ->
          if b == 0.0
            then Left (CompileError "division by zero in constant expression" Nothing Nothing)
            else Right (a / b)
        _ -> Left (CompileError "unsupported float assignment operation" Nothing Nothing)

    checkU32 n =
      when (n < 0 || n > fromIntegral (maxBound :: Word32)) $
        Left (CompileError "constant u32 is out of range" Nothing Nothing)

    checkI32 n =
      when (n < 0 || n > 0x7FFFFFFF) $
        Left (CompileError "constant i32 is out of range" Nothing Nothing)

evalConstFieldAccess :: ConstValue -> Text -> Either CompileError ConstValue
evalConstFieldAccess val field =
  case val of
    CVVector n scalar comps -> do
      idxs <- vectorFieldIndices field n
      case idxs of
        [ix] -> Right (comps !! fromIntegral ix)
        _ -> do
          let comps' = map (\ix -> comps !! fromIntegral ix) idxs
          Right (CVVector (length idxs) scalar comps')
    CVStruct _ fields ->
      case lookup field fields of
        Just v -> Right v
        Nothing -> Left (CompileError ("unknown field: " <> textToString field) Nothing Nothing)
    _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)

evalConstIndexAccess :: ConstValue -> ConstInt -> Either CompileError ConstValue
evalConstIndexAccess val (ConstInt _ raw) = do
  when (raw < 0) $
    Left (CompileError "index must be non-negative" Nothing Nothing)
  when (raw > fromIntegral (maxBound :: Int)) $
    Left (CompileError "index is out of range" Nothing Nothing)
  let ix = fromIntegral raw :: Int
  case val of
    CVVector n _ comps ->
      if ix < n then Right (comps !! ix) else Left (CompileError "vector index out of range" Nothing Nothing)
    CVMatrix cols _ _ colsVals ->
      if ix < cols then Right (colsVals !! ix) else Left (CompileError "matrix index out of range" Nothing Nothing)
    CVArray _ vals ->
      if ix < length vals then Right (vals !! ix) else Left (CompileError "array index out of range" Nothing Nothing)
    _ -> Left (CompileError "indexing requires array, vector, or matrix type" Nothing Nothing)

evalConstIntExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Expr -> Either CompileError ConstInt
evalConstIntExpr ctx constIndex fnIndex structIndex expr =
  evalConstIntExprWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr

evalConstIntExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstInt
evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EInt n -> do
          (scalar, val) <- selectIntLiteralScalar n
          case scalar of
            I32 -> do
              checkI32 val
              Right (ConstInt I32 val)
            U32 -> do
              checkU32 val
              Right (ConstInt U32 val)
            _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
        EUnary OpNeg inner -> do
          ConstInt scalar val <- go seen fnSeen inner
          case scalar of
            I32 -> do
              let v = negate val
              checkI32 v
              Right (ConstInt I32 v)
            U32 -> Left (CompileError "unary minus is not supported for u32 in const expressions" Nothing Nothing)
            _ -> Left (CompileError "unary minus expects integer constants" Nothing Nothing)
        EUnary OpDeref inner -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen (EUnary OpDeref inner)
          constValueToInt val
        EFloat _ ->
          Left (CompileError "const int expression references a float literal" Nothing Nothing)
        ECall "u32" [arg] ->
          case go seen fnSeen arg of
            Right (ConstInt _ n) -> do
              checkU32 n
              Right (ConstInt U32 n)
            _ -> do
              ConstFloat _ v <- evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen arg
              let n = truncate v :: Integer
              checkU32 n
              Right (ConstInt U32 n)
        ECall "i32" [arg] ->
          case go seen fnSeen arg of
            Right (ConstInt _ n) -> do
              checkI32 n
              Right (ConstInt I32 n)
            _ -> do
              ConstFloat _ v <- evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen arg
              let n = truncate v :: Integer
              checkI32 n
              Right (ConstInt I32 n)
        EBitcast _ _ ->
          Left (CompileError "bitcast is not allowed in const integer expressions" Nothing Nothing)
        ECall name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToInt val
        EBinary op a b -> do
          ConstInt sa va <- go seen fnSeen a
          ConstInt sb vb <- go seen fnSeen b
          (scalar, x, y) <- coercePair sa va sb vb
          case op of
            OpAdd -> applyIntOp scalar (+) x y
            OpSub -> applyIntOp scalar (-) x y
            OpMul -> applyIntOp scalar (*) x y
            OpDiv -> applyIntDiv scalar x y
            OpMod -> applyIntMod scalar x y
            OpBitAnd -> applyIntOp scalar (.&.) x y
            OpBitOr -> applyIntOp scalar (.|.) x y
            OpBitXor -> applyIntOp scalar xor x y
            OpShl -> applyShift scalar x y True
            OpShr -> applyShift scalar x y False
            _ -> Left (CompileError "unsupported const integer operation" Nothing Nothing)
        EVar name ->
          case Map.lookup name env of
            Just envBinding ->
              case cbValue envBinding of
                CVInt v -> Right v
                CVBool _ -> Left (CompileError "const int expression references a bool value" Nothing Nothing)
                CVFloat _ -> Left (CompileError "const int expression references a float value" Nothing Nothing)
                _ -> Left (CompileError "const int expression references a composite value" Nothing Nothing)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating constant selector" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToInt val
        EIndex _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToInt val
        _ -> Left (CompileError "switch case selector must be a constant integer expression" Nothing Nothing)

    checkU32 n =
      when (n < 0 || n > fromIntegral (maxBound :: Word32)) $
        Left (CompileError "switch case selector is out of range for u32" Nothing Nothing)

    checkI32 n =
      when (n < 0 || n > 0x7FFFFFFF) $
        Left (CompileError "switch case selector is out of range for i32" Nothing Nothing)

    coercePair sa va sb vb =
      case (sa, sb) of
        (I32, I32) -> Right (I32, va, vb)
        (U32, U32) -> Right (U32, va, vb)
        (I32, U32) ->
          if vb <= 0x7FFFFFFF
            then Right (I32, va, vb)
            else Left (CompileError "constant u32 is out of range for i32 operation" Nothing Nothing)
        (U32, I32) ->
          if va <= 0x7FFFFFFF
            then Right (I32, va, vb)
            else Left (CompileError "constant u32 is out of range for i32 operation" Nothing Nothing)
        _ -> Left (CompileError "unsupported const integer types" Nothing Nothing)

    applyIntOp scalar f x y =
      let v = f x y
      in case scalar of
          I32 -> checkI32 v >> Right (ConstInt I32 v)
          U32 -> checkU32 v >> Right (ConstInt U32 v)
          _ -> Left (CompileError "unsupported const integer operation" Nothing Nothing)

    applyIntDiv scalar x y =
      if y == 0
        then Left (CompileError "division by zero in constant expression" Nothing Nothing)
        else case scalar of
          I32 ->
            let v = (fromIntegral x :: Integer) `quot` (fromIntegral y :: Integer)
            in checkI32 v >> Right (ConstInt I32 v)
          U32 ->
            let v = (fromIntegral x :: Integer) `div` (fromIntegral y :: Integer)
            in checkU32 v >> Right (ConstInt U32 v)
          _ -> Left (CompileError "unsupported const integer division" Nothing Nothing)

    applyIntMod scalar x y =
      if y == 0
        then Left (CompileError "modulo by zero in constant expression" Nothing Nothing)
        else case scalar of
          I32 ->
            let v = (fromIntegral x :: Integer) `rem` (fromIntegral y :: Integer)
            in checkI32 v >> Right (ConstInt I32 v)
          U32 ->
            let v = (fromIntegral x :: Integer) `mod` (fromIntegral y :: Integer)
            in checkU32 v >> Right (ConstInt U32 v)
          _ -> Left (CompileError "unsupported const integer modulo" Nothing Nothing)

    applyShift scalar x y isLeft =
      if y < 0
        then Left (CompileError "shift amount must be non-negative" Nothing Nothing)
        else
          let v = if isLeft
                    then shiftL x (fromIntegral y)
                    else shiftR x (fromIntegral y)
          in case scalar of
              I32 -> checkI32 v >> Right (ConstInt I32 v)
              U32 -> checkU32 v >> Right (ConstInt U32 v)
              _ -> Left (CompileError "unsupported const integer shift" Nothing Nothing)

convertConstFloatTo :: Scalar -> ConstFloat -> ConstFloat
convertConstFloatTo target (ConstFloat _ v) =
  ConstFloat target (if target == F16 then quantizeF16 v else v)

quantizeF16 :: Double -> Double
quantizeF16 v =
  let f = realToFrac v :: Float
      bits = floatToHalfBits f
  in realToFrac (halfBitsToFloat bits) :: Double

evalConstFloatExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstFloat
evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EFloat f -> Right (ConstFloat F32 (realToFrac f))
        EInt n -> do
          (_, val) <- selectIntLiteralScalar n
          Right (ConstFloat F32 (fromIntegral val))
        EUnary OpNeg inner -> do
          cf <- go seen fnSeen inner
          Right (applyFloatOp (cfScalar cf) negate (cfValue cf))
        EUnary OpDeref inner -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen (EUnary OpDeref inner)
          constValueToFloat val
        ECall "f32" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (convertConstFloatTo F32 cf)
        ECall "f16" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (convertConstFloatTo F16 cf)
        ECall "abs" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (applyFloatOp (cfScalar cf) abs (cfValue cf))
        ECall "min" [a, b] -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          Right (applyFloatOp scalar (\v -> v) (min x y))
        ECall "max" [a, b] -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          Right (applyFloatOp scalar (\v -> v) (max x y))
        ECall "clamp" [xExpr, loExpr, hiExpr] -> do
          cfX <- evalFloatArg seen fnSeen xExpr
          cfLo <- evalFloatArg seen fnSeen loExpr
          cfHi <- evalFloatArg seen fnSeen hiExpr
          let (scalar1, x, lo) = coerceFloatPair cfX cfLo
          let hi = cfValue (convertConstFloatTo scalar1 cfHi)
          Right (applyFloatOp scalar1 (\v -> v) (min (max x lo) hi))
        ECall "mix" [aExpr, bExpr, tExpr] -> do
          cfA <- evalFloatArg seen fnSeen aExpr
          cfB <- evalFloatArg seen fnSeen bExpr
          cfT <- evalFloatArg seen fnSeen tExpr
          let (scalar1, a, b) = coerceFloatPair cfA cfB
          let t = cfValue (convertConstFloatTo scalar1 cfT)
          Right (applyFloatOp scalar1 (\v -> v) (a * (1.0 - t) + b * t))
        ECall "select" [aExpr, bExpr, condExpr] -> do
          cond <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen condExpr
          cfA <- evalFloatArg seen fnSeen aExpr
          cfB <- evalFloatArg seen fnSeen bExpr
          Right (if cond then cfB else cfA)
        ECall name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToFloat val
        EBinary op a b -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          case op of
            OpAdd -> Right (applyFloatOp scalar (\v -> v) (x + y))
            OpSub -> Right (applyFloatOp scalar (\v -> v) (x - y))
            OpMul -> Right (applyFloatOp scalar (\v -> v) (x * y))
            OpDiv ->
              if y == 0.0
                then Left (CompileError "division by zero in constant expression" Nothing Nothing)
                else Right (applyFloatOp scalar (\v -> v) (x / y))
            _ -> Left (CompileError "unsupported const float operation" Nothing Nothing)
        EVar name ->
          case Map.lookup name env of
            Just envBinding ->
              case cbValue envBinding of
                CVFloat v -> Right v
                CVInt (ConstInt _ v) -> Right (ConstFloat F32 (fromIntegral v))
                CVBool _ -> Left (CompileError "const float expression references a bool value" Nothing Nothing)
                _ -> Left (CompileError "const float expression references a composite value" Nothing Nothing)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating const float expression" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        EIndex _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        _ -> Left (CompileError "const float expression requires a constant float expression" Nothing Nothing)

    evalFloatArg seen fnSeen ex = do
      cf <- go seen fnSeen ex
      Right cf

    coerceFloatPair a b =
      let scalar = if cfScalar a == F32 || cfScalar b == F32 then F32 else F16
          a' = convertConstFloatTo scalar a
          b' = convertConstFloatTo scalar b
      in (scalar, cfValue a', cfValue b')

    applyFloatOp scalar f v =
      let v' = f v
      in ConstFloat scalar (if scalar == F16 then quantizeF16 v' else v')

evalConstBoolExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Expr -> Either CompileError Bool
evalConstBoolExpr ctx constIndex fnIndex structIndex expr =
  evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr

evalConstBoolExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError Bool
evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr = go seenConsts seenFns expr
  where
    go seen fnSeen ex =
      case ex of
        EBool b -> Right b
        EUnary OpNot a -> not <$> go seen fnSeen a
        EUnary OpDeref inner -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen (EUnary OpDeref inner)
          constValueToBool val
        EBinary OpAnd a b -> (&&) <$> go seen fnSeen a <*> go seen fnSeen b
        EBinary OpOr a b -> (||) <$> go seen fnSeen a <*> go seen fnSeen b
        EBinary OpEq a b -> evalEq seen fnSeen a b
        EBinary OpNe a b -> not <$> evalEq seen fnSeen a b
        EBinary OpLt a b -> evalCmp seen fnSeen (<) (<) a b
        EBinary OpLe a b -> evalCmp seen fnSeen (<=) (<=) a b
        EBinary OpGt a b -> evalCmp seen fnSeen (>) (>) a b
        EBinary OpGe a b -> evalCmp seen fnSeen (>=) (>=) a b
        ECall "select" [aExpr, bExpr, condExpr] -> do
          cond <- go seen fnSeen condExpr
          if cond then go seen fnSeen bExpr else go seen fnSeen aExpr
        EBitcast _ _ ->
          Left (CompileError "bitcast is not allowed in const boolean expressions" Nothing Nothing)
        ECall name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToBool val
        EVar name ->
          case Map.lookup name env of
            Just envBinding ->
              case cbValue envBinding of
                CVBool b -> Right b
                CVInt _ -> Left (CompileError "const bool expression references an int value" Nothing Nothing)
                CVFloat _ -> Left (CompileError "const bool expression references a float value" Nothing Nothing)
                _ -> Left (CompileError "const bool expression references a composite value" Nothing Nothing)
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating const_assert" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just expr' -> evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
        EField _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToBool val
        EIndex _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToBool val
        _ -> Left (CompileError "const_assert requires a constant boolean expression" Nothing Nothing)

    evalEq seen fnSeen a b =
      case (go seen fnSeen a, go seen fnSeen b) of
        (Right x, Right y) -> Right (x == y)
        _ ->
          let valA = evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
              valB = evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
              isPointerConst v =
                case v of
                  CVPointer _ _ -> True
                  _ -> False
              isCompositeConst v =
                case v of
                  CVVector _ _ _ -> True
                  CVMatrix _ _ _ _ -> True
                  CVArray _ _ -> True
                  CVStruct _ _ -> True
                  _ -> False
          in case (valA, valB) of
              (Right va, Right vb)
                | isPointerConst va || isPointerConst vb ->
                    Left (CompileError "const_assert cannot compare pointer values" Nothing Nothing)
                | isCompositeConst va || isCompositeConst vb ->
                    if constValueType va == constValueType vb
                      then Right (va == vb)
                      else Left (CompileError "const_assert comparison requires matching types" Nothing Nothing)
              _ ->
                let intA = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
                    intB = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
                in case (intA, intB) of
                    (Right (ConstInt _ x), Right (ConstInt _ y)) -> Right (x == y)
                    _ ->
                      let floatA = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
                          floatB = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
                      in case (floatA, floatB) of
                          (Right (ConstFloat _ x), Right (ConstFloat _ y)) -> Right (x == y)
                          _ -> Left (firstError intA intB)

    evalCmp seen fnSeen cmpInt cmpFloat a b =
      let intA = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
          intB = evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
      in case (intA, intB) of
          (Right (ConstInt _ x), Right (ConstInt _ y)) -> Right (cmpInt x y)
          _ ->
            let floatA = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen a
                floatB = evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen b
            in case (floatA, floatB) of
                (Right (ConstFloat _ x), Right (ConstFloat _ y)) -> Right (cmpFloat x y)
                _ -> Left (firstError intA intB)

    firstError ea eb =
      case ea of
        Left err -> err
        _ ->
          case eb of
            Left err -> err
            _ -> CompileError "const_assert requires a constant boolean expression" Nothing Nothing

coerceConstIntToScalar :: Scalar -> ConstInt -> Either CompileError ConstInt
coerceConstIntToScalar target (ConstInt scalar val) =
  case (target, scalar) of
    (I32, I32) -> Right (ConstInt I32 val)
    (U32, U32) -> Right (ConstInt U32 val)
    (I32, U32) ->
      if val <= 0x7FFFFFFF
        then Right (ConstInt I32 val)
        else Left (CompileError "constant u32 is out of range for i32" Nothing Nothing)
    (U32, I32) ->
      if val >= 0 && val <= fromIntegral (maxBound :: Word32)
        then Right (ConstInt U32 val)
        else Left (CompileError "constant i32 is out of range for u32" Nothing Nothing)
    _ -> Left (CompileError "unsupported const integer coercion" Nothing Nothing)


resolveConstRef :: ModuleContext -> Text -> Either CompileError ([Text], Text)
resolveConstRef ctx name =
  case splitQName name of
    [] -> Left (CompileError "invalid constant reference" Nothing Nothing)
    [single] ->
      if Set.member single (mcConstNames ctx)
        then Right (mcPath ctx, single)
        else case Map.lookup single (mcItemAliases ctx) of
          Just target ->
            case splitLast target of
              Nothing -> Left (CompileError "invalid constant reference" Nothing Nothing)
              Just (path, item) -> Right (path, item)
          Nothing -> Left (CompileError ("unknown constant: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid constant reference" Nothing Nothing)
            Just (path, item) -> Right (path, item)

resolveFunctionRef :: ModuleContext -> Text -> Either CompileError ([Text], Text)
resolveFunctionRef ctx name =
  case splitQName name of
    [] -> Left (CompileError "invalid function reference" Nothing Nothing)
    [single] ->
      if Set.member single (mcFunctionNames ctx)
        then Right (mcPath ctx, single)
        else case Map.lookup single (mcItemAliases ctx) of
          Just target ->
            case splitLast target of
              Nothing -> Left (CompileError "invalid function reference" Nothing Nothing)
              Just (path, item) -> Right (path, item)
          Nothing -> Left (CompileError ("unknown function: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid function reference" Nothing Nothing)
            Just (path, item) -> Right (path, item)

validateLValue :: ModuleContext -> Scope -> LValue -> Either CompileError ()
validateLValue ctx scope lv =
  case lv of
    LVVar name -> validateName ctx scope name
    LVField base _ -> validateLValue ctx scope base
    LVIndex base idx -> validateLValue ctx scope base >> validateExpr ctx scope idx
    LVDeref expr -> validateExpr ctx scope expr

validateExpr :: ModuleContext -> Scope -> Expr -> Either CompileError ()
validateExpr ctx scope expr =
  case expr of
    EVar name -> validateName ctx scope name
    EInt _ -> Right ()
    EFloat _ -> Right ()
    EBool _ -> Right ()
    EBinary _ a b -> validateExpr ctx scope a >> validateExpr ctx scope b
    EUnary OpAddr a ->
      case exprToLValue a of
        Nothing -> Left (CompileError "address-of requires an addressable expression" Nothing Nothing)
        Just _ -> validateExpr ctx scope a
    EUnary OpDeref a -> validateExpr ctx scope a
    EUnary _ a -> validateExpr ctx scope a
    ECall name args -> validateName ctx scope name >> mapM_ (validateExpr ctx scope) args
    EBitcast ty arg -> validateType ctx scope ty >> validateExpr ctx scope arg
    EField base _ -> validateExpr ctx scope base
    EIndex base idx -> validateExpr ctx scope base >> validateExpr ctx scope idx

validateType :: ModuleContext -> Scope -> Type -> Either CompileError ()
validateType ctx scope ty =
  case ty of
    TyStructRef name ->
      case lookupNameId scope name of
        Just sid | IntSet.member sid (scTypeAliases scope) -> Right ()
        _ -> validateName ctx scope name
    TyArray elemTy _ -> validateType ctx scope elemTy
    TyVector _ _ -> Right ()
    TyMatrix _ _ _ -> Right ()
    TyTexture1D _ -> Right ()
    TyTexture1DArray _ -> Right ()
    TyTexture2D _ -> Right ()
    TyTexture2DArray _ -> Right ()
    TyTexture3D _ -> Right ()
    TyTextureCube _ -> Right ()
    TyTextureCubeArray _ -> Right ()
    TyTextureMultisampled2D _ -> Right ()
    TyTextureDepth2D -> Right ()
    TyTextureDepth2DArray -> Right ()
    TyTextureDepthCube -> Right ()
    TyTextureDepthCubeArray -> Right ()
    TyTextureDepthMultisampled2D -> Right ()
    TyStorageTexture1D _ _ -> Right ()
    TyStorageTexture2D _ _ -> Right ()
    TyStorageTexture2DArray _ _ -> Right ()
    TyStorageTexture3D _ _ -> Right ()
    TyAtomic _ -> Right ()
    TyPtr _ _ inner -> validateType ctx scope inner
    TySampler -> Right ()
    TySamplerComparison -> Right ()
    TyScalar _ -> Right ()

validateName :: ModuleContext -> Scope -> Text -> Either CompileError ()
validateName _ scope name =
  case splitQName name of
    [] -> Right ()
    [single] ->
      if isBuiltinName single
        then Right ()
        else
          case lookupNameId scope single of
            Just sid
              | IntSet.member sid (scopeLocals scope)
                  || IntSet.member sid (scGlobals scope)
                  || IntSet.member sid (scItemAliases scope) -> Right ()
            _ -> Left (CompileError ("unknown identifier: " <> textToString single) Nothing Nothing)
    seg0 : _ ->
      case lookupNameId scope seg0 of
        Just sid | IntSet.member sid (scModuleAliases scope) -> Right ()
        _ -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)

lookupNameId :: Scope -> Text -> Maybe Int
lookupNameId scope name = Map.lookup name (ntMap (scNameTable scope))

scopeLocals :: Scope -> IntSet.IntSet
scopeLocals scope = foldl' IntSet.union IntSet.empty (scScopes scope)

currentScope :: Scope -> IntSet.IntSet
currentScope scope =
  case scScopes scope of
    [] -> IntSet.empty
    s : _ -> s

scopeAdd :: Scope -> Text -> Either CompileError Scope
scopeAdd scope name =
  let (nameId, table') = internName name (scNameTable scope)
      scope' = scope { scNameTable = table' }
  in if IntSet.member nameId (currentScope scope')
      then Left (CompileError ("duplicate local declaration: " <> textToString name) Nothing Nothing)
      else
        if not (scAllowShadowing scope')
          && not (T.isPrefixOf "_" name)
          && IntSet.member nameId (scopeOuterLocals scope')
          then Left (CompileError ("shadowing is not allowed: " <> textToString name) Nothing Nothing)
          else
            case scScopes scope' of
              [] ->
                Right scope' { scScopes = [IntSet.singleton nameId] }
              current : rest ->
                Right scope' { scScopes = IntSet.insert nameId current : rest }

scopeOuterLocals :: Scope -> IntSet.IntSet
scopeOuterLocals scope =
  case scScopes scope of
    [] -> IntSet.empty
    _ : rest -> foldl' IntSet.union IntSet.empty rest

enterBlock :: Scope -> Scope
enterBlock scope = scope { scScopes = IntSet.empty : scScopes scope }

scopeWithParams :: Scope -> [Text] -> Scope
scopeWithParams scope names =
  let (table', ids) = internNames (scNameTable scope) names
  in scope { scNameTable = table', scScopes = [IntSet.fromList ids] }

ensureNoDuplicates :: Text -> [Text] -> Either CompileError ()
ensureNoDuplicates label names =
  let (dups, _) = foldl' collect ([], Set.empty) names
  in if null dups
      then Right ()
      else Left (CompileError ("duplicate " <> textToString label <> ": " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)

isBuiltinName :: Text -> Bool
isBuiltinName name =
  name `Set.member` builtinNames || maybe False (const True) (parseMatrixName name)

builtinNames :: Set.Set Text
builtinNames =
  Set.fromList
    [ "vec2"
    , "vec3"
    , "vec4"
    , "array"
    , "f16"
    , "f32"
    , "u32"
    , "i32"
    , "abs"
    , "min"
    , "max"
    , "clamp"
    , "mix"
    , "select"
    , "any"
    , "all"
    , "round"
    , "roundEven"
    , "trunc"
    , "step"
    , "smoothstep"
    , "floor"
    , "ceil"
    , "fract"
    , "radians"
    , "degrees"
    , "exp"
    , "log"
    , "exp2"
    , "log2"
    , "sin"
    , "cos"
    , "tan"
    , "asin"
    , "acos"
    , "atan"
    , "atan2"
    , "sinh"
    , "cosh"
    , "tanh"
    , "asinh"
    , "acosh"
    , "atanh"
    , "pow"
    , "sqrt"
    , "inverseSqrt"
    , "fma"
    , "sign"
    , "length"
    , "normalize"
    , "dot"
    , "cross"
    , "distance"
    , "faceForward"
    , "reflect"
    , "refract"
    , "transpose"
    , "determinant"
    , "inverse"
    , "modf"
    , "frexp"
    , "ldexp"
    , "pack4x8snorm"
    , "pack4x8unorm"
    , "pack2x16snorm"
    , "pack2x16unorm"
    , "pack2x16float"
    , "unpack4x8snorm"
    , "unpack4x8unorm"
    , "unpack2x16snorm"
    , "unpack2x16unorm"
    , "unpack2x16float"
    , "firstLeadingBit"
    , "firstTrailingBit"
    , "saturate"
    , "quantizeToF16"
    , "countOneBits"
    , "countLeadingZeros"
    , "countTrailingZeros"
    , "reverseBits"
    , "extractBits"
    , "insertBits"
    , "dot4U8Packed"
    , "dot4I8Packed"
    , "arrayLength"
    , "textureSample"
    , "textureSampleCompare"
    , "textureSampleLevel"
    , "textureSampleBias"
    , "textureSampleGrad"
    , "textureSampleCompareLevel"
    , "textureGather"
    , "textureGatherCompare"
    , "textureDimensions"
    , "textureNumLevels"
    , "textureNumLayers"
    , "textureNumSamples"
    , "textureLoad"
    , "textureStore"
    , "dpdx"
    , "dpdy"
    , "fwidth"
    , "atomicLoad"
    , "atomicStore"
    , "atomicAdd"
    , "atomicSub"
    , "atomicMin"
    , "atomicMax"
    , "atomicAnd"
    , "atomicOr"
    , "atomicXor"
    , "atomicExchange"
    ]

qualifyModule :: [Text] -> Map.Map FilePath ModuleContext -> ModuleNode -> ModuleNode
qualifyModule rootPath ctxs node =
  let ctx = fromMaybe (buildModuleContext rootPath "" node) (Map.lookup (mnFile node) ctxs)
      ast = mnAst node
      prefix = mcPath ctx
      rename = qualNameWithRoot (mcRootPath ctx) prefix
      renameDecl name = rename name
      rewriteTy = rewriteType ctx
      rewriteParam = rewriteParamType ctx
      rewriteField = rewriteFieldDecl ctx
      rewriteStmt = rewriteStmtNames ctx
      rewriteExpr = rewriteExprNames ctx
      rewriteFn fn =
        fn
          { fnName = renameDecl (fnName fn)
          , fnParams = map rewriteParam (fnParams fn)
          , fnReturnType = fmap rewriteTy (fnReturnType fn)
          , fnBody = map rewriteStmt (fnBody fn)
          }
      rewriteConst c =
        c
          { cdName = renameDecl (cdName c)
          , cdExpr = rewriteExpr (cdExpr c)
          }
      rewriteAlias a =
        a
          { adName = renameDecl (adName a)
          , adType = rewriteTy (adType a)
          }
      rewriteOverride o =
        o
          { odName = renameDecl (odName o)
          , odType = rewriteTy (odType o)
          , odExpr = fmap rewriteExpr (odExpr o)
          }
      rewriteBinding b =
        b
          { bdName = renameDecl (bdName b)
          , bdType = rewriteTy (bdType b)
          }
      rewriteGlobal g =
        g
          { gvName = renameDecl (gvName g)
          , gvType = rewriteTy (gvType g)
          , gvInit = fmap rewriteExpr (gvInit g)
          }
      rewriteStruct s =
        s
          { sdName = renameDecl (sdName s)
          , sdFields = map rewriteField (sdFields s)
          }
      rewriteConstAssert (ConstAssert pos expr) =
        ConstAssert pos (rewriteExpr expr)
      entry =
        case modEntry ast of
          Nothing -> Nothing
          Just e ->
            if prefix == rootPath
              then Just
                e
                  { epParams = map rewriteParam (epParams e)
                  , epReturnType = fmap rewriteTy (epReturnType e)
                  , epBody = map rewriteStmt (epBody e)
                  }
              else Nothing
      ast' =
        ast
          { modImports = []
          , modAliases = map rewriteAlias (modAliases ast)
          , modStructs = map rewriteStruct (modStructs ast)
          , modBindings = map rewriteBinding (modBindings ast)
          , modGlobals = map rewriteGlobal (modGlobals ast)
          , modConsts = map rewriteConst (modConsts ast)
          , modOverrides = map rewriteOverride (modOverrides ast)
          , modConstAsserts = map rewriteConstAssert (modConstAsserts ast)
          , modFunctions = map rewriteFn (modFunctions ast)
          , modEntry = entry
          }
  in node { mnAst = ast' }

qualNameWithRoot :: [Text] -> [Text] -> Text -> Text
qualNameWithRoot rootPath path name =
  if null path || path == rootPath
    then name
    else "__wesl__" <> T.intercalate "__" path <> "__" <> name

rewriteIdent :: ModuleContext -> Text -> Text
rewriteIdent ctx name =
  case splitQName name of
    [] -> name
    [single] ->
      if Set.member single (mcLocals ctx)
        then
          if mcPath ctx == mcRootPath ctx
            then single
            else qualNameWithRoot (mcRootPath ctx) (mcPath ctx) single
        else
          case Map.lookup single (mcItemAliases ctx) of
            Just target ->
              case splitLast target of
                Nothing -> single
                Just (path, item) -> qualNameWithRoot (mcRootPath ctx) path item
            Nothing -> single
    seg0 : segRest ->
      case Map.lookup seg0 (mcModuleAliases ctx) of
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> name
            Just (path, item) -> qualNameWithRoot (mcRootPath ctx) path item
        Nothing ->
          case splitLast (seg0 : segRest) of
            Nothing -> name
            Just (path, item) -> qualNameWithRoot (mcRootPath ctx) path item

rewriteType :: ModuleContext -> Type -> Type
rewriteType ctx ty =
  case ty of
    TyStructRef name -> TyStructRef (rewriteIdent ctx name)
    TyArray elemTy n -> TyArray (rewriteType ctx elemTy) n
    TyVector n s -> TyVector n s
    TyMatrix c r s -> TyMatrix c r s
    TyTexture1D s -> TyTexture1D s
    TyTexture1DArray s -> TyTexture1DArray s
    TyTexture2D s -> TyTexture2D s
    TyTexture2DArray s -> TyTexture2DArray s
    TyTexture3D s -> TyTexture3D s
    TyTextureCube s -> TyTextureCube s
    TyTextureCubeArray s -> TyTextureCubeArray s
    TyTextureMultisampled2D s -> TyTextureMultisampled2D s
    TyTextureDepth2D -> TyTextureDepth2D
    TyTextureDepth2DArray -> TyTextureDepth2DArray
    TyTextureDepthCube -> TyTextureDepthCube
    TyTextureDepthCubeArray -> TyTextureDepthCubeArray
    TyTextureDepthMultisampled2D -> TyTextureDepthMultisampled2D
    TyStorageTexture1D fmt acc -> TyStorageTexture1D fmt acc
    TyStorageTexture2D fmt acc -> TyStorageTexture2D fmt acc
    TyStorageTexture2DArray fmt acc -> TyStorageTexture2DArray fmt acc
    TyStorageTexture3D fmt acc -> TyStorageTexture3D fmt acc
    TyAtomic s -> TyAtomic s
    TyPtr addr access inner -> TyPtr addr access (rewriteType ctx inner)
    TySampler -> TySampler
    TySamplerComparison -> TySamplerComparison
    TyScalar s -> TyScalar s

rewriteParamType :: ModuleContext -> Param -> Param
rewriteParamType ctx param =
  param { paramType = rewriteType ctx (paramType param) }

rewriteFieldDecl :: ModuleContext -> FieldDecl -> FieldDecl
rewriteFieldDecl ctx field =
  field { fdType = rewriteType ctx (fdType field) }

rewriteStmtNames :: ModuleContext -> Stmt -> Stmt
rewriteStmtNames ctx stmt =
  case stmt of
    SLet name expr -> SLet name (rewriteExprNames ctx expr)
    SVar name expr -> SVar name (rewriteExprNames ctx expr)
    SAssign lv expr -> SAssign (rewriteLValueNames ctx lv) (rewriteExprNames ctx expr)
    SAssignOp lv op expr -> SAssignOp (rewriteLValueNames ctx lv) op (rewriteExprNames ctx expr)
    SInc lv -> SInc (rewriteLValueNames ctx lv)
    SDec lv -> SDec (rewriteLValueNames ctx lv)
    SExpr expr -> SExpr (rewriteExprNames ctx expr)
    SIf cond thenBody elseBody ->
      SIf (rewriteExprNames ctx cond)
          (map (rewriteStmtNames ctx) thenBody)
          (fmap (map (rewriteStmtNames ctx)) elseBody)
    SWhile cond body ->
      SWhile (rewriteExprNames ctx cond) (map (rewriteStmtNames ctx) body)
    SLoop body continuing ->
      SLoop (map (rewriteStmtNames ctx) body) (fmap (map (rewriteStmtNames ctx)) continuing)
    SFor initStmt condExpr contStmt body ->
      SFor (fmap (rewriteStmtNames ctx) initStmt)
           (fmap (rewriteExprNames ctx) condExpr)
           (fmap (rewriteStmtNames ctx) contStmt)
           (map (rewriteStmtNames ctx) body)
    SSwitch expr cases defBody ->
      SSwitch (rewriteExprNames ctx expr)
        (map rewriteCase cases)
        (fmap (map (rewriteStmtNames ctx)) defBody)
    SBreak -> SBreak
    SBreakIf cond -> SBreakIf (rewriteExprNames ctx cond)
    SContinue -> SContinue
    SDiscard -> SDiscard
    SFallthrough -> SFallthrough
    SReturn expr -> SReturn (fmap (rewriteExprNames ctx) expr)
  where
    rewriteCase sc =
      sc { scSelectors = map (rewriteExprNames ctx) (scSelectors sc)
         , scBody = map (rewriteStmtNames ctx) (scBody sc)
         }

rewriteLValueNames :: ModuleContext -> LValue -> LValue
rewriteLValueNames ctx lv =
  case lv of
    LVVar name -> LVVar (rewriteIdent ctx name)
    LVField base field -> LVField (rewriteLValueNames ctx base) field
    LVIndex base idx -> LVIndex (rewriteLValueNames ctx base) (rewriteExprNames ctx idx)
    LVDeref expr -> LVDeref (rewriteExprNames ctx expr)

rewriteExprNames :: ModuleContext -> Expr -> Expr
rewriteExprNames ctx expr =
  case expr of
    EVar name -> EVar (rewriteIdent ctx name)
    EInt _ -> expr
    EFloat _ -> expr
    EBool _ -> expr
    EBinary op a b -> EBinary op (rewriteExprNames ctx a) (rewriteExprNames ctx b)
    EUnary op a -> EUnary op (rewriteExprNames ctx a)
    ECall name args -> ECall (rewriteIdent ctx name) (map (rewriteExprNames ctx) args)
    EBitcast ty arg -> EBitcast (rewriteType ctx ty) (rewriteExprNames ctx arg)
    EField base field -> EField (rewriteExprNames ctx base) field
    EIndex base idx -> EIndex (rewriteExprNames ctx base) (rewriteExprNames ctx idx)

splitQName :: Text -> [Text]
splitQName name =
  let (headSeg, rest) = T.breakOn "::" name
  in if T.null rest
       then [headSeg]
       else headSeg : splitQName (T.drop 2 rest)

splitLast :: [a] -> Maybe ([a], a)
splitLast xs =
  case xs of
    [] -> Nothing
    [x] -> Just ([], x)
    (x:rest) -> do
      (prefix, lastVal) <- splitLast rest
      Just (x : prefix, lastVal)

builtinInputType :: Stage -> Text -> Maybe Type
builtinInputType stage name =
  case (stage, name) of
    (StageCompute, "global_invocation_id") -> Just (TyVector 3 U32)
    (StageCompute, "local_invocation_id") -> Just (TyVector 3 U32)
    (StageCompute, "workgroup_id") -> Just (TyVector 3 U32)
    (StageCompute, "local_invocation_index") -> Just (TyScalar U32)
    (StageCompute, "num_workgroups") -> Just (TyVector 3 U32)
    (StageVertex, "vertex_index") -> Just (TyScalar U32)
    (StageVertex, "instance_index") -> Just (TyScalar U32)
    (StageFragment, "position") -> Just (TyVector 4 F32)
    (StageFragment, "front_facing") -> Just (TyScalar Bool)
    (StageFragment, "sample_index") -> Just (TyScalar U32)
    _ -> Nothing

builtinOutputType :: Stage -> Text -> Maybe Type
builtinOutputType stage name =
  case (stage, name) of
    (StageVertex, "position") -> Just (TyVector 4 F32)
    (StageFragment, "frag_depth") -> Just (TyScalar F32)
    _ -> Nothing

builtinInputDecoration :: Stage -> Text -> Maybe Word32
builtinInputDecoration stage name =
  case (stage, name) of
    (StageCompute, "global_invocation_id") -> Just builtInGlobalInvocationId
    (StageCompute, "local_invocation_id") -> Just builtInLocalInvocationId
    (StageCompute, "workgroup_id") -> Just builtInWorkgroupId
    (StageCompute, "local_invocation_index") -> Just builtInLocalInvocationIndex
    (StageCompute, "num_workgroups") -> Just builtInNumWorkgroups
    (StageVertex, "vertex_index") -> Just builtInVertexIndex
    (StageVertex, "instance_index") -> Just builtInInstanceIndex
    (StageFragment, "position") -> Just builtInFragCoord
    (StageFragment, "front_facing") -> Just builtInFrontFacing
    (StageFragment, "sample_index") -> Just builtInSampleIndex
    _ -> Nothing

builtinOutputDecoration :: Stage -> Text -> Maybe Word32
builtinOutputDecoration stage name =
  case (stage, name) of
    (StageVertex, "position") -> Just builtInPosition
    (StageFragment, "frag_depth") -> Just builtInFragDepth
    _ -> Nothing
