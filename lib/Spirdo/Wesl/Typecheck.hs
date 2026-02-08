{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Typechecking, validation, and import resolution.
module Spirdo.Wesl.Typecheck where

import Control.Monad (foldM, zipWithM, unless, when, replicateM)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
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
  case (entry.epStage, entry.epWorkgroupSize) of
    (StageCompute, Nothing) -> Left (CompileError "@workgroup_size is required for @compute" Nothing Nothing)
    (StageCompute, Just _) -> pure ()
    (StageVertex, Nothing) -> pure ()
    (StageVertex, Just _) -> Left (CompileError "@workgroup_size is not allowed for @vertex" Nothing Nothing)
    (StageFragment, Nothing) -> pure ()
    (StageFragment, Just _) -> Left (CompileError "@workgroup_size is not allowed for @fragment" Nothing Nothing)

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
emptyModuleAst = ModuleAst [] [] [] [] [] [] [] [] [] [] []

resolveImports :: CompileOptions -> FilePath -> ModuleAst -> IO (Either CompileError ModuleAst)
resolveImports opts rootFile rootAst = runExceptT $ do
  let rootDir = takeDirectory rootFile
  graph <- ExceptT (loadModuleGraph opts rootDir rootFile rootAst)
  liftEither (linkModules opts rootFile rootDir graph)
  where
    liftEither = either throwE pure

-- | Resolve imports for inline modules using an in-memory module map.
resolveImportsInline :: CompileOptions -> FilePath -> ModuleAst -> Map.Map FilePath ModuleAst -> Either CompileError ([ModuleNode], ModuleAst)
resolveImportsInline opts rootFile rootAst moduleMap = do
  let rootDir = takeDirectory rootFile
  graph <- loadModuleGraphInline opts rootDir rootFile rootAst moduleMap
  linked <- linkModules opts rootFile rootDir graph
  pure (graph, linked)

loadModuleGraphInline :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> Map.Map FilePath ModuleAst -> Either CompileError [ModuleNode]
loadModuleGraphInline opts rootDir rootFile rootAst moduleMap = go Map.empty (Seq.singleton (rootFile, rootAst))
  where
    go acc queue =
      case Seq.viewl queue of
        Seq.EmptyL -> Right (Map.elems acc)
        (filePath, ast) Seq.:< queueRest -> do
          let pathSegs = modulePathFromFile rootDir filePath
          imports <- resolveImportItemsInline opts rootDir filePath ast moduleMap
          validateImportAliases imports
          let node = ModuleNode filePath pathSegs ast imports
          let acc' = Map.insert filePath node acc
          targets <- loadImportTargets acc' imports
          go acc' (queueRest <> Seq.fromList targets)

    loadImportTargets acc' imports = do
      let moduleFiles = Map.keys (Map.fromList [(imp.irModuleFile, ()) | imp <- imports])
      targets <- sequence (map (loadOne acc') moduleFiles)
      pure (concat targets)

    loadOne acc' moduleFile =
      if Map.member moduleFile acc'
        then Right []
        else case Map.lookup moduleFile moduleMap of
          Nothing ->
            Left (CompileError ("import module not found: " <> moduleFile) Nothing Nothing)
          Just ast' ->
            Right [(moduleFile, ast')]

resolveImportItemsInline :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> Map.Map FilePath ModuleAst -> Either CompileError [ImportResolved]
resolveImportItemsInline opts rootDir moduleFile ast moduleMap =
  let items = [(decl, item) | decl <- ast.modImports, item <- decl.idItems]
  in mapM (uncurry (resolveImportItemInline opts rootDir moduleFile moduleMap)) items

resolveImportItemInline :: CompileOptions -> FilePath -> FilePath -> Map.Map FilePath ModuleAst -> ImportDecl -> ImportItem -> Either CompileError ImportResolved
resolveImportItemInline opts rootDir moduleFile moduleMap decl item = do
  let baseDir = importBaseDir rootDir moduleFile decl.idRelative
  let segs = item.iiPath
  let fullBase = appendPathSegments baseDir segs
  let fullMod = findModuleInline moduleMap fullBase
  case fullMod of
    Just moduleBase -> resolveAsModule baseDir segs moduleBase
    Nothing -> resolveAsItem baseDir segs
  where
    resolveAsModule baseDir' segs' moduleBase = do
      let ambiguous = ambiguousImport opts baseDir' segs'
      when ambiguous $
        Left (CompileError ("ambiguous import: " <> renderPath segs' <> " refers to both a module and an item") Nothing Nothing)
      pure (ImportResolved (modulePathFromFile rootDir moduleBase) moduleBase Nothing item.iiAlias)

    resolveAsItem baseDir' segs' =
      case splitImportTarget segs' of
        Nothing -> Left (importNotFound segs')
        Just (modSegs, itemName) -> do
          let moduleBasePath = appendPathSegments baseDir' modSegs
          case findModuleInline moduleMap moduleBasePath of
            Just mb ->
              Right (ImportResolved (modulePathFromFile rootDir mb) mb (Just itemName) item.iiAlias)
            Nothing ->
              Left (importNotFound modSegs)

    ambiguousImport _opts baseDir' segs' =
      case splitImportTarget segs' of
        Nothing -> False
        Just (modSegs, itemName) ->
          case findModuleInline moduleMap (appendPathSegments baseDir' modSegs) of
            Nothing -> False
            Just mb -> moduleHasItemInline moduleMap mb itemName

    importNotFound segs' =
      case segs' of
        [] -> CompileError "import path is empty" Nothing Nothing
        _ -> CompileError ("import module not found: " <> renderPath segs') Nothing Nothing

findModuleInline :: Map.Map FilePath ModuleAst -> FilePath -> Maybe FilePath
findModuleInline moduleMap base =
  case Map.lookup base moduleMap of
    Just _ -> Just base
    Nothing -> Nothing

moduleHasItemInline :: Map.Map FilePath ModuleAst -> FilePath -> Text -> Bool
moduleHasItemInline moduleMap moduleBase itemName =
  case Map.lookup moduleBase moduleMap of
    Nothing -> False
    Just ast ->
      Set.member itemName (moduleItemNames ast)
  where
    moduleItemNames ast =
      Set.fromList $
        map (.sdName) ast.modStructs
          <> map (.bdName) ast.modBindings
          <> map (.gvName) ast.modGlobals
          <> map (.cdName) ast.modConsts
          <> map (.odName) ast.modOverrides
          <> map (.adName) ast.modAliases
          <> map (.fnName) ast.modFunctions
          <> map (.epName) ast.modEntries

loadModuleGraph :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> IO (Either CompileError [ModuleNode])
loadModuleGraph opts rootDir rootFile rootAst = runExceptT (go Map.empty (Seq.singleton (rootFile, rootAst)))
  where
    go acc queue =
      case Seq.viewl queue of
        Seq.EmptyL -> pure (Map.elems acc)
        (filePath, ast) Seq.:< queueRest -> do
          let pathSegs = modulePathFromFile rootDir filePath
          imports <- ExceptT (resolveImportItems opts rootDir filePath ast)
          liftEither (validateImportAliases imports)
          let node = ModuleNode filePath pathSegs ast imports
          let acc' = Map.insert filePath node acc
          targets <- loadImportTargets opts acc' imports
          go acc' (queueRest <> Seq.fromList targets)

    loadImportTargets opts' acc' imports = do
      let moduleFiles = Map.keys (Map.fromList [(imp.irModuleFile, ()) | imp <- imports])
      results <- liftIO (mapM (loadOne opts' acc') moduleFiles)
      targets <- liftEither (sequence results)
      pure (concat targets)

    loadOne opts' acc' moduleFile =
      if Map.member moduleFile acc'
        then pure (Right [])
        else do
          moduleAst <- loadModuleFromFile opts' moduleFile
          pure (fmap (\ast' -> [(moduleFile, ast')]) moduleAst)

    liftEither :: Either CompileError a -> ExceptT CompileError IO a
    liftEither = either throwE pure

validateImportAliases :: [ImportResolved] -> Either CompileError ()
validateImportAliases imports =
  let aliases = mapMaybe aliasFor imports
      (dups, _) = foldl' collect ([], Set.empty) aliases
      targets = map importTarget imports
      (dupTargets, _) = foldl' collect ([], Set.empty) targets
  in do
      unless (null dups) $
        Left (CompileError ("duplicate import aliases: " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
      unless (null dupTargets) $
        Left (CompileError ("duplicate imports: " <> T.unpack (T.intercalate ", " dupTargets)) Nothing Nothing)
  where
    aliasFor imp =
      let alias =
            case imp.irItem of
              Nothing -> fromMaybe (last imp.irModulePath) imp.irAlias
              Just item -> fromMaybe item imp.irAlias
      in nonEmptyText alias

    importTarget imp =
      let modName = T.intercalate "::" imp.irModulePath
      in case imp.irItem of
          Nothing -> modName
          Just item -> modName <> "::" <> item

    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)

    nonEmptyText txt
      | T.null txt = Nothing
      | otherwise = Just txt

resolveImportItems :: CompileOptions -> FilePath -> FilePath -> ModuleAst -> IO (Either CompileError [ImportResolved])
resolveImportItems opts rootDir moduleFile ast = runExceptT $ do
  let items = [(decl, item) | decl <- ast.modImports, item <- decl.idItems]
  mapM (ExceptT . resolveOne) items
  where
    resolveOne (decl, item) = resolveImportItem opts rootDir moduleFile decl item

loadModuleFromFile :: CompileOptions -> FilePath -> IO (Either CompileError ModuleAst)
loadModuleFromFile opts path = do
  sourceFile <- pickFirstExisting [path <.> "wesl", path <.> "wgsl"]
  maybe (pure (Right emptyModuleAst)) parseModule sourceFile
  where
    parseModule file = do
      src <- readFile file
      pure (parseModuleWith opts.enabledFeatures src)

pickFirstExisting :: [FilePath] -> IO (Maybe FilePath)
pickFirstExisting files =
  case files of
    [] -> pure Nothing
    (file:rest) -> do
      exists <- doesFileExist file
      if exists
        then pure (Just file)
        else pickFirstExisting rest

resolveImportItem :: CompileOptions -> FilePath -> FilePath -> ImportDecl -> ImportItem -> IO (Either CompileError ImportResolved)
resolveImportItem opts rootDir moduleFile decl item = runExceptT $ do
  let baseDir = importBaseDir rootDir moduleFile decl.idRelative
  let segs = item.iiPath
  let fullBase = appendPathSegments baseDir segs
  fullMod <- liftIO (findModuleFile fullBase)
  case fullMod of
    Just moduleBase -> resolveAsModule baseDir segs moduleBase
    Nothing -> resolveAsItem baseDir segs
  where
    resolveAsModule baseDir' segs' moduleBase = do
      ambiguous <- liftIO (ambiguousImport opts baseDir' segs')
      when ambiguous $
        throwE (CompileError ("ambiguous import: " <> renderPath segs' <> " refers to both a module and an item") Nothing Nothing)
      pure (ImportResolved (modulePathFromFile rootDir moduleBase) moduleBase Nothing item.iiAlias)

    resolveAsItem baseDir' segs' =
      case splitImportTarget segs' of
        Nothing -> throwE (importNotFound segs')
        Just (modSegs, itemName) -> do
          let moduleBasePath = appendPathSegments baseDir' modSegs
          moduleBase <- liftIO (findModuleFile moduleBasePath)
          case moduleBase of
            Just mb ->
              pure (ImportResolved (modulePathFromFile rootDir mb) mb (Just itemName) item.iiAlias)
            Nothing ->
              throwE (importNotFound modSegs)

    ambiguousImport opts' baseDir' segs' =
      case splitImportTarget segs' of
        Nothing -> pure False
        Just (modSegs, itemName) -> do
          let moduleBasePath = appendPathSegments baseDir' modSegs
          moduleBaseItem <- findModuleFile moduleBasePath
          maybe (pure False) (\mb -> moduleHasItem opts' mb itemName) moduleBaseItem

    importNotFound segs' =
      case segs' of
        [] -> CompileError "import path is empty" Nothing Nothing
        _ -> CompileError ("import module not found: " <> renderPath segs') Nothing Nothing

moduleHasItem :: CompileOptions -> FilePath -> Text -> IO Bool
moduleHasItem opts moduleBase itemName = do
  astResult <- loadModuleFromFile opts moduleBase
  pure (either (const False) (Set.member itemName . moduleItemNames) astResult)
  where
    moduleItemNames ast =
      Set.fromList $
        map (.sdName) ast.modStructs
          <> map (.bdName) ast.modBindings
          <> map (.gvName) ast.modGlobals
          <> map (.cdName) ast.modConsts
          <> map (.odName) ast.modOverrides
          <> map (.adName) ast.modAliases
          <> map (.fnName) ast.modFunctions
          <> map (.epName) ast.modEntries

findModuleFile :: FilePath -> IO (Maybe FilePath)
findModuleFile base = do
  sourceFile <- pickFirstExisting [base <.> "wesl", base <.> "wgsl"]
  pure (base <$ sourceFile)

appendPathSegments :: FilePath -> [Text] -> FilePath
appendPathSegments base segs = foldl (</>) base (map T.unpack segs)

splitImportTarget :: [Text] -> Maybe ([Text], Text)
splitImportTarget segs =
  case reverse segs of
    itemName : revModSegs@(_ : _) -> Just (reverse revModSegs, itemName)
    _ -> Nothing

renderPath :: [Text] -> String
renderPath = T.unpack . T.intercalate "::"

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
  rootNode <- case listToMaybe [n | n <- nodes, n.mnFile == rootFile] of
    Just n -> Right n
    Nothing -> Left (CompileError "root module not found during import resolution" Nothing Nothing)
  let rootPath = rootNode.mnPath
  let otherEntries = [n | n <- nodes, n.mnFile /= rootFile, not (null n.mnAst.modEntries)]
  unless (null otherEntries) $
    Left (CompileError "entry points are only supported in the root module" Nothing Nothing)
  let constIndex = buildConstIndex nodes
  let fnIndex = buildFunctionIndex nodes
  let structIndex = buildStructIndex nodes
  let overrideIndex = buildOverrideIndex nodes
  validateModuleScopes opts True rootPath rootDir constIndex fnIndex structIndex overrideIndex nodes
  let contexts = Map.fromList [(n.mnFile, buildModuleContext rootPath rootDir n) | n <- nodes]
  let qualified = map (qualifyModule rootPath contexts) nodes
  let merged = foldr mergeModule emptyModuleAst qualified
  let rootEntries = concat [q.mnAst.modEntries | q <- qualified, q.mnFile == rootFile]
  Right merged { modEntries = rootEntries }

mergeModule :: ModuleNode -> ModuleAst -> ModuleAst
mergeModule node acc =
  let ast = node.mnAst
  in acc
      { modDirectives = ast.modDirectives <> acc.modDirectives
      , modAliases = ast.modAliases <> acc.modAliases
      , modStructs = ast.modStructs <> acc.modStructs
      , modBindings = ast.modBindings <> acc.modBindings
      , modGlobals = ast.modGlobals <> acc.modGlobals
      , modConsts = ast.modConsts <> acc.modConsts
      , modOverrides = ast.modOverrides <> acc.modOverrides
      , modConstAsserts = ast.modConstAsserts <> acc.modConstAsserts
      , modFunctions = ast.modFunctions <> acc.modFunctions
      , modEntries = ast.modEntries <> acc.modEntries
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
  case Map.lookup name table.ntMap of
    Just i -> (i, table)
    Nothing ->
      let i = table.ntNext
          table' = table { ntNext = i + 1, ntMap = Map.insert name i table.ntMap }
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
  let ast = node.mnAst
      localNames =
        Set.fromList
          ( map (.sdName) ast.modStructs
            <> map (.bdName) ast.modBindings
            <> map (.gvName) ast.modGlobals
            <> map (.cdName) ast.modConsts
            <> map (.odName) ast.modOverrides
            <> map (.fnName) ast.modFunctions
            <> map (.epName) ast.modEntries
          )
      constNames = Set.fromList (map (.cdName) ast.modConsts <> map (.odName) ast.modOverrides)
      functionNames = Set.fromList (map (.fnName) ast.modFunctions)
      (moduleAliases, itemAliases) = buildAliasMaps node.mnImports
  in ModuleContext node.mnPath moduleAliases itemAliases localNames constNames functionNames rootPath

data ConstIndex = ConstIndex
  { ciPathTable :: !NameTable
  , ciNameTable :: !NameTable
  , ciEntries :: !(IntMap.IntMap (IntMap.IntMap (Maybe Type, Expr)))
  }

buildConstIndex :: [ModuleNode] -> ConstIndex
buildConstIndex nodes =
  let (pt, nt, entries) = foldl' addNode (emptyNameTable, emptyNameTable, IntMap.empty) nodes
  in ConstIndex pt nt entries
  where
    addNode (pt, nt, acc) node =
      let (pid, pt1) = internName (pathKey node.mnPath) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) (constPairs node)
          merged = IntMap.union entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, entry) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insert nid entry m)
    constPairs n =
      let ast = n.mnAst
          consts = [(c.cdName, (c.cdType, c.cdExpr)) | c <- ast.modConsts]
          overrides = [(o.odName, (o.odType, expr)) | o <- ast.modOverrides, Just expr <- [o.odExpr]]
      in consts <> overrides

lookupConstIndex :: ConstIndex -> [Text] -> Text -> Maybe (Maybe Type, Expr)
lookupConstIndex idx path name = do
  pid <- Map.lookup (pathKey path) idx.ciPathTable.ntMap
  nid <- Map.lookup name idx.ciNameTable.ntMap
  IntMap.lookup pid idx.ciEntries >>= IntMap.lookup nid

type OverrideIndex = Map.Map [Text] (Set.Set Text)

buildOverrideIndex :: [ModuleNode] -> OverrideIndex
buildOverrideIndex nodes =
  Map.fromList
    [ (n.mnPath, Set.fromList (map (.odName) n.mnAst.modOverrides))
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
      let (pid, pt1) = internName (pathKey node.mnPath) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) [(s.sdName, s) | s <- node.mnAst.modStructs]
          merged = IntMap.union entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, decl) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insert nid decl m)

lookupStructIndex :: StructIndex -> [Text] -> Text -> Maybe StructDecl
lookupStructIndex idx path name = do
  pid <- Map.lookup (pathKey path) idx.siPathTable.ntMap
  nid <- Map.lookup name idx.siNameTable.ntMap
  IntMap.lookup pid idx.siEntries >>= IntMap.lookup nid

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
      let (pid, pt1) = internName (pathKey node.mnPath) pt
          (nt1, entryMap) = foldl' addEntry (nt, IntMap.empty) [(f.fnName, [f]) | f <- node.mnAst.modFunctions]
          merged = IntMap.unionWith (<>) entryMap (IntMap.findWithDefault IntMap.empty pid acc)
      in (pt1, nt1, IntMap.insert pid merged acc)
    addEntry (nt0, m) (name, decls) =
      let (nid, nt1) = internName name nt0
      in (nt1, IntMap.insertWith (<>) nid decls m)

lookupFunctionIndex :: FunctionIndex -> [Text] -> Text -> Maybe [FunctionDecl]
lookupFunctionIndex idx path name = do
  pid <- Map.lookup (pathKey path) idx.fiPathTable.ntMap
  nid <- Map.lookup name idx.fiNameTable.ntMap
  IntMap.lookup pid idx.fiEntries >>= IntMap.lookup nid

lowerOverridesWith :: [Text] -> [(Text, OverrideValue)] -> ModuleAst -> Either CompileError ModuleAst
lowerOverridesWith rootPath overridesMap ast =
  case ast.modOverrides of
    [] -> Right ast
    overrides -> do
      let existing = Set.fromList (map (.cdName) ast.modConsts)
      let (dups, _) = foldl' collect ([], Set.empty) (map (.odName) overrides)
      unless (null dups) $
        Left (CompileError ("duplicate override declarations: " <> T.unpack (T.intercalate ", " dups)) Nothing Nothing)
      when (any ((`Set.member` existing) . (.odName)) overrides) $
        Left (CompileError "override names must not conflict with const declarations" Nothing Nothing)
      let overrideLookup = buildOverrideValueMap rootPath overridesMap
      let structEnv = [(s.sdName, s) | s <- ast.modStructs]
      (consts, kept) <- foldM (partitionOverride structEnv overrideLookup) ([], []) overrides
      Right ast { modConsts = ast.modConsts <> reverse consts, modOverrides = reverse kept }
  where
    collect (acc, seen) name =
      if Set.member name seen
        then (name : acc, seen)
        else (acc, Set.insert name seen)
    partitionOverride structEnv overrideLookup (constAcc, keepAcc) o =
      case Map.lookup o.odName overrideLookup of
        Just ov -> do
          ty <- case o.odType of
            Nothing -> Left (CompileError "override value requires a concrete override type" Nothing Nothing)
            Just t -> Right t
          expr <- overrideValueToExpr structEnv ty ov
          Right (ConstDecl o.odName (Just ty) expr : constAcc, keepAcc)
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
        OVBool b -> Right (EBool syntheticPos b)
        _ -> Left (CompileError "override value must be a bool" Nothing Nothing)
    TyScalar I32 ->
      case ov of
        OVI32 v -> Right (EInt syntheticPos v)
        _ -> Left (CompileError "override value must be an i32" Nothing Nothing)
    TyScalar U32 ->
      case ov of
        OVU32 v -> Right (ECall syntheticPos "u32" [EInt syntheticPos v])
        _ -> Left (CompileError "override value must be a u32" Nothing Nothing)
    TyScalar F32 ->
      case ov of
        OVF32 v -> Right (EFloat syntheticPos v)
        OVF16 v -> Right (ECall syntheticPos "f32" [EFloat syntheticPos v])
        _ -> Left (CompileError "override value must be an f32" Nothing Nothing)
    TyScalar F16 ->
      case ov of
        OVF16 v -> Right (ECall syntheticPos "f16" [EFloat syntheticPos v])
        OVF32 v -> Right (ECall syntheticPos "f16" [EFloat syntheticPos v])
        _ -> Left (CompileError "override value must be an f16" Nothing Nothing)
    TyVector n scalar ->
      case ov of
        OVComposite vals -> do
          when (length vals /= n) $
            Left (CompileError "override vector arity does not match" Nothing Nothing)
          args <- mapM (overrideValueToExpr structEnv (TyScalar scalar)) vals
          Right (ECall syntheticPos ("vec" <> T.pack (show n)) args)
        _ -> Left (CompileError "override value must be a vector" Nothing Nothing)
    TyMatrix cols rows scalar ->
      case ov of
        OVComposite colsVals -> do
          when (length colsVals /= cols) $
            Left (CompileError "override matrix column count does not match" Nothing Nothing)
          let colTy = TyVector rows scalar
          args <- mapM (overrideValueToExpr structEnv colTy) colsVals
          Right (ECall syntheticPos ("mat" <> T.pack (show cols) <> "x" <> T.pack (show rows)) args)
        _ -> Left (CompileError "override value must be a matrix" Nothing Nothing)
    TyArray elemTy (ArrayLenFixed n) ->
      case ov of
        OVComposite vals -> do
          when (length vals /= n) $
            Left (CompileError "override array length does not match" Nothing Nothing)
          args <- mapM (overrideValueToExpr structEnv elemTy) vals
          Right (ECall syntheticPos "array" args)
        _ -> Left (CompileError "override value must be an array" Nothing Nothing)
    TyArray _ ArrayLenRuntime ->
      Left (CompileError "override values cannot target runtime-sized arrays" Nothing Nothing)
    TyArray _ (ArrayLenExpr _) ->
      Left (CompileError "override values require a fixed array length" Nothing Nothing)
    TyStructRef name ->
      case lookup name structEnv of
        Nothing -> Left (CompileError ("unknown struct: " <> T.unpack name) Nothing Nothing)
        Just decl ->
          case ov of
            OVComposite vals -> do
              let fields = decl.sdFields
              when (length vals /= length fields) $
                Left (CompileError "override struct field count does not match" Nothing Nothing)
              args <- zipWithM (\f v -> overrideValueToExpr structEnv f.fdType v) fields vals
              Right (ECall syntheticPos name args)
            _ -> Left (CompileError "override value must be a struct composite" Nothing Nothing)
    _ -> Left (CompileError "override values are only supported for scalar, vector, matrix, array, and struct types" Nothing Nothing)

resolveTypeAliases :: ModuleAst -> Either CompileError ModuleAst
resolveTypeAliases ast = do
  let (dupAliases, _) = foldl' collect ([], Set.empty) (map (.adName) ast.modAliases)
  unless (null dupAliases) $
    Left (CompileError ("duplicate type aliases: " <> T.unpack (T.intercalate ", " dupAliases)) Nothing Nothing)
  let aliasMap = Map.fromList [(a.adName, a.adType) | a <- ast.modAliases]
  let expand = expandType aliasMap
  aliases <- mapM (\a -> (\ty -> a { adType = ty }) <$> expand a.adType) ast.modAliases
  structs <- mapM (expandStruct expand) ast.modStructs
  bindings <- mapM (expandBinding expand) ast.modBindings
  globals <- mapM (expandGlobal expand) ast.modGlobals
  consts <- mapM (expandConst expand) ast.modConsts
  overrides <- mapM (expandOverride expand) ast.modOverrides
  constAsserts <- mapM (expandConstAssert expand) ast.modConstAsserts
  functions <- mapM (expandFunction expand) ast.modFunctions
  entries <- mapM (expandEntry expand) ast.modEntries
  Right ast
    { modAliases = aliases
    , modStructs = structs
    , modBindings = bindings
    , modGlobals = globals
    , modConsts = consts
    , modOverrides = overrides
    , modConstAsserts = constAsserts
    , modFunctions = functions
    , modEntries = entries
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
      fields <- mapM (\f -> (\ty -> f { fdType = ty }) <$> expand f.fdType) decl.sdFields
      Right decl { sdFields = fields }

    expandBinding expand decl = do
      ty <- expand decl.bdType
      Right decl { bdType = ty }

    expandGlobal expand decl = do
      ty <- expand decl.gvType
      init' <- mapM (expandExpr expand) decl.gvInit
      Right decl { gvType = ty, gvInit = init' }

    expandConstAssert expand (ConstAssert pos expr) = do
      expr' <- expandExpr expand expr
      Right (ConstAssert pos expr')

    expandConst expand decl = do
      ty <- mapM expand decl.cdType
      expr <- expandExpr expand decl.cdExpr
      Right decl { cdType = ty, cdExpr = expr }

    expandOverride expand decl = do
      ty <- mapM expand decl.odType
      expr <- mapM (expandExpr expand) decl.odExpr
      Right decl { odType = ty, odExpr = expr }

    expandFunction expand decl = do
      params <- mapM (\p -> (\ty -> p { paramType = ty }) <$> expand p.paramType) decl.fnParams
      ret <- mapM expand decl.fnReturnType
      body <- mapM (expandStmt expand) decl.fnBody
      Right decl { fnParams = params, fnReturnType = ret, fnBody = body }

    expandEntry expand decl = do
      params <- mapM (\p -> (\ty -> p { paramType = ty }) <$> expand p.paramType) decl.epParams
      ret <- mapM expand decl.epReturnType
      body <- mapM (expandStmt expand) decl.epBody
      Right decl { epParams = params, epReturnType = ret, epBody = body }

    expandStmt expand stmt =
      case stmt of
        SLet pos name mType expr -> SLet pos name <$> mapM expand mType <*> expandExpr expand expr
        SVar pos name mType mExpr -> SVar pos name <$> mapM expand mType <*> mapM (expandExpr expand) mExpr
        SAssign pos lv expr -> SAssign pos <$> expandLValue expand lv <*> expandExpr expand expr
        SAssignOp pos lv op expr -> SAssignOp pos <$> expandLValue expand lv <*> pure op <*> expandExpr expand expr
        SInc pos lv -> SInc pos <$> expandLValue expand lv
        SDec pos lv -> SDec pos <$> expandLValue expand lv
        SExpr pos expr -> SExpr pos <$> expandExpr expand expr
        SIf pos cond thenBody elseBody ->
          SIf pos <$> expandExpr expand cond <*> mapM (expandStmt expand) thenBody <*> mapM (mapM (expandStmt expand)) elseBody
        SWhile pos cond body -> SWhile pos <$> expandExpr expand cond <*> mapM (expandStmt expand) body
        SLoop pos body cont -> SLoop pos <$> mapM (expandStmt expand) body <*> mapM (mapM (expandStmt expand)) cont
        SFor pos initStmt cond cont body ->
          SFor pos <$> mapM (expandStmt expand) initStmt <*> mapM (expandExpr expand) cond <*> mapM (expandStmt expand) cont <*> mapM (expandStmt expand) body
        SSwitch pos expr cases defBody ->
          SSwitch pos <$> expandExpr expand expr <*> mapM (expandCase expand) cases <*> mapM (mapM (expandStmt expand)) defBody
        SBreak pos -> Right (SBreak pos)
        SBreakIf pos expr -> SBreakIf pos <$> expandExpr expand expr
        SContinue pos -> Right (SContinue pos)
        SDiscard pos -> Right (SDiscard pos)
        SFallthrough pos -> Right (SFallthrough pos)
        SReturn pos expr -> SReturn pos <$> mapM (expandExpr expand) expr

    expandCase expand (SwitchCase sels body) =
      SwitchCase <$> mapM (expandExpr expand) sels <*> mapM (expandStmt expand) body

    expandLValue expand lv =
      case lv of
        LVVar pos name -> Right (LVVar pos name)
        LVField pos inner field -> LVField pos <$> expandLValue expand inner <*> pure field
        LVIndex pos inner idx -> LVIndex pos <$> expandLValue expand inner <*> expandExpr expand idx
        LVDeref pos expr -> LVDeref pos <$> expandExpr expand expr

    expandExpr expand expr =
      case expr of
        EBinary pos op a b -> EBinary pos op <$> expandExpr expand a <*> expandExpr expand b
        EUnary pos op a -> EUnary pos op <$> expandExpr expand a
        ECall pos name args -> ECall pos name <$> mapM (expandExpr expand) args
        EBitcast pos ty arg -> EBitcast pos <$> expand ty <*> expandExpr expand arg
        EField pos base field -> EField pos <$> expandExpr expand base <*> pure field
        EIndex pos base idx -> EIndex pos <$> expandExpr expand base <*> expandExpr expand idx
        _ -> Right expr

inferOverrideTypes :: [Text] -> FilePath -> ModuleAst -> Either CompileError ModuleAst
inferOverrideTypes rootPath rootDir ast = do
  let node = ModuleNode "<inline>" rootPath ast []
  let ctx = buildModuleContext rootPath rootDir node
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  overrides <- mapM (inferOverride ctx constIndex fnIndex structIndex) ast.modOverrides
  Right ast { modOverrides = overrides }
  where
    inferOverride ctx constIndex fnIndex structIndex o =
      case o.odType of
        Just _ -> Right o
        Nothing ->
          case o.odExpr of
            Nothing -> Left (CompileError "override declarations require a type or initializer" Nothing Nothing)
            Just expr -> do
              val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty expr
              let ty = constValueType val
              Right o { odType = Just ty }

resolveConstExprs :: [Text] -> FilePath -> ModuleAst -> Either CompileError ModuleAst
resolveConstExprs rootPath rootDir ast = do
  let node = ModuleNode "<inline>" rootPath ast []
  let ctx = buildModuleContext rootPath rootDir node
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  aliases <- mapM (resolveAlias ctx constIndex fnIndex structIndex) ast.modAliases
  structs <- mapM (resolveStruct ctx constIndex fnIndex structIndex) ast.modStructs
  bindings <- mapM (resolveBinding ctx constIndex fnIndex structIndex) ast.modBindings
  globals <- mapM (resolveGlobal ctx constIndex fnIndex structIndex) ast.modGlobals
  consts <- mapM (resolveConst ctx constIndex fnIndex structIndex) ast.modConsts
  overrides <- mapM (resolveOverride ctx constIndex fnIndex structIndex) ast.modOverrides
  functions <- mapM (resolveFunction ctx constIndex fnIndex structIndex) ast.modFunctions
  entries <- mapM (resolveEntry ctx constIndex fnIndex structIndex) ast.modEntries
  Right ast
    { modAliases = aliases
    , modStructs = structs
    , modBindings = bindings
    , modGlobals = globals
    , modConsts = consts
    , modOverrides = overrides
    , modFunctions = functions
    , modEntries = entries
    }
  where
    resolveArrayLen ctx constIndex fnIndex structIndex len =
      case len of
        ArrayLenRuntime -> Right ArrayLenRuntime
        ArrayLenFixed n -> Right (ArrayLenFixed n)
        ArrayLenExpr expr -> do
          ConstInt _ v <- evalConstIntExpr ctx constIndex fnIndex structIndex (constExprToExpr expr)
          when (v <= 0) $
            Left (CompileError "array length must be positive" Nothing Nothing)
          when (v > fromIntegral (maxBound :: Int)) $
            Left (CompileError "array length is too large" Nothing Nothing)
          Right (ArrayLenFixed (fromIntegral v))

    resolveType ctx constIndex fnIndex structIndex ty =
      case ty of
        TyArray elemTy len -> do
          elemTy' <- resolveType ctx constIndex fnIndex structIndex elemTy
          len' <- resolveArrayLen ctx constIndex fnIndex structIndex len
          Right (TyArray elemTy' len')
        TyPtr addr access elemTy ->
          TyPtr addr access <$> resolveType ctx constIndex fnIndex structIndex elemTy
        _ -> Right ty

    resolveAlias ctx constIndex fnIndex structIndex decl = do
      ty <- resolveType ctx constIndex fnIndex structIndex decl.adType
      Right decl { adType = ty }

    resolveStruct ctx constIndex fnIndex structIndex decl = do
      fields <- mapM (resolveField ctx constIndex fnIndex structIndex) decl.sdFields
      Right decl { sdFields = fields }

    resolveField ctx constIndex fnIndex structIndex fld = do
      ty <- resolveType ctx constIndex fnIndex structIndex fld.fdType
      attrs <- resolveAttrs ctx constIndex fnIndex structIndex fld.fdAttrs
      Right fld { fdType = ty, fdAttrs = attrs }

    resolveBinding ctx constIndex fnIndex structIndex decl = do
      ty <- resolveType ctx constIndex fnIndex structIndex decl.bdType
      Right decl { bdType = ty }

    resolveGlobal ctx constIndex fnIndex structIndex decl = do
      ty <- resolveType ctx constIndex fnIndex structIndex decl.gvType
      Right decl { gvType = ty }

    resolveConst ctx constIndex fnIndex structIndex decl = do
      ty <- mapM (resolveType ctx constIndex fnIndex structIndex) decl.cdType
      Right decl { cdType = ty }

    resolveOverride ctx constIndex fnIndex structIndex decl = do
      ty <- mapM (resolveType ctx constIndex fnIndex structIndex) decl.odType
      Right decl { odType = ty }

    resolveParam ctx constIndex fnIndex structIndex param = do
      ty <- resolveType ctx constIndex fnIndex structIndex param.paramType
      attrs <- resolveAttrs ctx constIndex fnIndex structIndex param.paramAttrs
      Right param { paramType = ty, paramAttrs = attrs }

    resolveFunction ctx constIndex fnIndex structIndex fn = do
      params <- mapM (resolveParam ctx constIndex fnIndex structIndex) fn.fnParams
      retTy <- mapM (resolveType ctx constIndex fnIndex structIndex) fn.fnReturnType
      Right fn { fnParams = params, fnReturnType = retTy }

    resolveEntry ctx constIndex fnIndex structIndex entry = do
      params <- mapM (resolveParam ctx constIndex fnIndex structIndex) entry.epParams
      retTy <- mapM (resolveType ctx constIndex fnIndex structIndex) entry.epReturnType
      wg <- resolveWorkgroup ctx constIndex fnIndex structIndex entry.epStage entry.epWorkgroupSize
      Right entry { epParams = params, epReturnType = retTy, epWorkgroupSize = wg }

    resolveAttrs ctx constIndex fnIndex structIndex attrs =
      mapM (resolveAttr ctx constIndex fnIndex structIndex) attrs

    resolveAttr ctx constIndex fnIndex structIndex attr =
      case attr of
        Attr name args
          | name `elem` ["align", "size", "location"] -> do
              args' <- mapM (resolveAttrInt ctx constIndex fnIndex structIndex) args
              Right (Attr name args')
          | otherwise -> Right attr
        AttrIf _ -> Right attr

    resolveAttrInt ctx constIndex fnIndex structIndex arg =
      case arg of
        AttrInt v -> Right (AttrInt v)
        AttrIdent name -> do
          ConstInt _ v <- evalConstIntExpr ctx constIndex fnIndex structIndex (EVar syntheticPos name)
          Right (AttrInt v)
        AttrExpr expr -> do
          ConstInt _ v <- evalConstIntExpr ctx constIndex fnIndex structIndex (constExprToExpr expr)
          Right (AttrInt v)

    resolveWorkgroup ctx constIndex fnIndex structIndex stage wg =
      case (stage, wg) of
        (StageCompute, Nothing) ->
          Left (CompileError "@workgroup_size is required for @compute" Nothing Nothing)
        (StageCompute, Just (WorkgroupSizeExpr exprs)) -> do
          when (null exprs || length exprs > 3) $
            Left (CompileError "@workgroup_size expects 1, 2, or 3 values" Nothing Nothing)
          vals <- mapM (evalConstIntExpr ctx constIndex fnIndex structIndex . constExprToExpr) exprs
          let ints = map (\(ConstInt _ v) -> v) vals
          when (any (<= 0) ints) $
            Left (CompileError "@workgroup_size values must be positive" Nothing Nothing)
          when (any (> fromIntegral (maxBound :: Word32)) ints) $
            Left (CompileError "@workgroup_size value is too large" Nothing Nothing)
          let xs = map fromIntegral ints
          let (x, y, z) =
                case xs of
                  [a] -> (a, 1, 1)
                  [a, b] -> (a, b, 1)
                  [a, b, c] -> (a, b, c)
                  _ -> (1, 1, 1)
          Right (Just (WorkgroupSizeValue (x, y, z)))
        (StageCompute, Just (WorkgroupSizeValue _)) -> Right wg
        (StageVertex, Nothing) -> Right Nothing
        (StageFragment, Nothing) -> Right Nothing
        (StageVertex, Just _) ->
          Left (CompileError "@workgroup_size is not allowed for @vertex" Nothing Nothing)
        (StageFragment, Just _) ->
          Left (CompileError "@workgroup_size is not allowed for @fragment" Nothing Nothing)

constExprToExpr :: ConstExpr -> Expr
constExprToExpr expr =
  case expr of
    CEInt n -> EInt syntheticPos n
    CEIdent name -> EVar syntheticPos name
    CEUnaryNeg inner -> EUnary syntheticPos OpNeg (constExprToExpr inner)
    CEBinary op a b -> EBinary syntheticPos (constBinOpToBinOp op) (constExprToExpr a) (constExprToExpr b)
    CECall name args -> ECall syntheticPos name (map constExprToExpr args)

constBinOpToBinOp :: ConstBinOp -> BinOp
constBinOpToBinOp op =
  case op of
    CAdd -> OpAdd
    CSub -> OpSub
    CMul -> OpMul
    CDiv -> OpDiv
    CMod -> OpMod
    CShl -> OpShl
    CShr -> OpShr
    CBitAnd -> OpBitAnd
    CBitOr -> OpBitOr
    CBitXor -> OpBitXor

buildAliasMaps :: [ImportResolved] -> (Map.Map Text [Text], Map.Map Text [Text])
buildAliasMaps = foldl' add (Map.empty, Map.empty)
  where
    add (modAcc, itemAcc) imp =
      case imp.irItem of
        Nothing ->
          let alias = fromMaybe (last imp.irModulePath) imp.irAlias
          in if T.null alias
              then (modAcc, itemAcc)
              else (Map.insert alias imp.irModulePath modAcc, itemAcc)
        Just item ->
          let alias = fromMaybe item imp.irAlias
              target = imp.irModulePath <> [item]
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
    enabled = Set.fromList (map T.pack opts.enabledFeatures)
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
validateModuleScopes opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex =
  mapM_ (validateModuleScope opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex)

validateModuleScope :: CompileOptions -> Bool -> [Text] -> FilePath -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> ModuleNode -> Either CompileError ()
validateModuleScope opts skipConstAsserts rootPath rootDir constIndex fnIndex structIndex overrideIndex node = do
  let ctx = buildModuleContext rootPath rootDir node
  diagConfig <- validateDirectives opts node.mnAst.modDirectives
  let allowShadowing = diagnosticSeverity diagConfig "shadowing" /= DiagError
  let (nt1, globalsIds) = internNameSet emptyNameTable (Set.toList ctx.mcLocals)
  let (nt2, moduleAliasIds) = internNameSet nt1 (Map.keys ctx.mcModuleAliases)
  let (nt3, itemAliasIds) = internNameSet nt2 (Map.keys ctx.mcItemAliases)
  let (nt4, typeAliasIds) = internNameSet nt3 (map (.adName) node.mnAst.modAliases)
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
  validateModuleAst ctx constIndex fnIndex structIndex overrideIndex scope0 diagConfig skipConstAsserts node.mnAst

type DiagnosticConfig = Map.Map String DiagnosticSeverity

validateModuleAst :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> OverrideIndex -> Scope -> DiagnosticConfig -> Bool -> ModuleAst -> Either CompileError ()
validateModuleAst ctx constIndex fnIndex structIndex overrideIndex scope diagConfig skipConstAsserts ast = do
  mapM_ (validateAlias ctx scope) ast.modAliases
  mapM_ (validateStruct ctx scope) ast.modStructs
  mapM_ (validateBinding ctx scope) ast.modBindings
  mapM_ (validateGlobalVar ctx scope) ast.modGlobals
  mapM_ (validateConst ctx constIndex fnIndex structIndex scope) ast.modConsts
  mapM_ (validateOverride ctx scope overrideIndex) ast.modOverrides
  unless skipConstAsserts $
    mapM_ (validateConstAssert ctx constIndex fnIndex structIndex diagConfig) ast.modConstAsserts
  mapM_ (validateFunction ctx constIndex fnIndex structIndex skipConstAsserts scope) ast.modFunctions
  mapM_ (validateEntryPoint ctx constIndex fnIndex structIndex skipConstAsserts scope) ast.modEntries

validateConstAssertsMerged :: CompileOptions -> [Text] -> ModuleAst -> Either CompileError ()
validateConstAssertsMerged opts rootPath ast = do
  let node = ModuleNode "<merged>" rootPath ast []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext rootPath "" node
  diagConfig <- validateDirectives opts ast.modDirectives
  mapM_ (validateConstAssert ctx constIndex fnIndex structIndex diagConfig) ast.modConstAsserts
  Right ()

collectDiagnosticsMerged :: CompileOptions -> [Text] -> ModuleAst -> Either CompileError [Diagnostic]
collectDiagnosticsMerged opts rootPath ast = do
  let node = ModuleNode "<merged>" rootPath ast []
  let constIndex = buildConstIndex [node]
  let fnIndex = buildFunctionIndex [node]
  let structIndex = buildStructIndex [node]
  let ctx = buildModuleContext rootPath "" node
  diagConfig <- validateDirectives opts ast.modDirectives
  constDiags <- fmap concat (mapM (collectConstAssertDiagnostic ctx constIndex fnIndex structIndex diagConfig) ast.modConstAsserts)
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
        else Right [Diagnostic sev "const_assert" "const_assert condition failed" (Just pos.spLine) (Just pos.spCol)]

collectUnreachableDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnreachableDiagnostics diagConfig ast =
  case Map.lookup "unreachable_code" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag pos = Diagnostic sev "unreachable_code" "unreachable code" (Just pos.spLine) (Just pos.spCol)
          bodies = map (.fnBody) ast.modFunctions <> map (.epBody) ast.modEntries
      in concatMap (unreachableInStmts mkDiag) bodies
  where
    unreachableInStmts mkDiag = go False
      where
        go _ [] = []
        go unreachable (stmt:rest) =
          let here = [mkDiag (stmtPos stmt) | unreachable]
              nested =
                case stmt of
                  SIf _ _ thenBody elseBody ->
                    unreachableInStmts mkDiag thenBody <> maybe [] (unreachableInStmts mkDiag) elseBody
                  SWhile _ _ body ->
                    unreachableInStmts mkDiag body
                  SLoop _ body continuing ->
                    unreachableInStmts mkDiag body <> maybe [] (unreachableInStmts mkDiag) continuing
                  SFor _ _ _ _ body ->
                    unreachableInStmts mkDiag body
                  SSwitch _ _ cases defBody ->
                    concatMap (unreachableInStmts mkDiag . (.scBody)) cases <> maybe [] (unreachableInStmts mkDiag) defBody
                  _ -> []
              unreachable' = unreachable || isTerminator stmt
          in here <> nested <> go unreachable' rest

    isTerminator stmt =
      case stmt of
        SReturn _ _ -> True
        SBreak _ -> True
        SContinue _ -> True
        SDiscard _ -> True
        _ -> False

collectUnusedExpressionDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedExpressionDiagnostics diagConfig ast =
  case Map.lookup "unused_expression" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag pos = Diagnostic sev "unused_expression" "expression has no effect" (Just pos.spLine) (Just pos.spCol)
          bodies = map (.fnBody) ast.modFunctions <> map (.epBody) ast.modEntries
      in concatMap (unusedExprs mkDiag) bodies
  where
    unusedExprs mkDiag = go
      where
        go [] = []
        go (stmt:rest) =
          let here =
                case stmt of
                  SExpr _ expr ->
                    case expr of
                      ECall _ _ _ -> []
                      _ -> [mkDiag (exprPos expr)]
                  _ -> []
              nested =
                case stmt of
                  SIf _ _ thenBody elseBody ->
                    go thenBody <> maybe [] go elseBody
                  SWhile _ _ body ->
                    go body
                  SLoop _ body continuing ->
                    go body <> maybe [] go continuing
                  SFor _ _ _ _ body ->
                    go body
                  SSwitch _ _ cases defBody ->
                    concatMap (go . (.scBody)) cases <> maybe [] go defBody
                  _ -> []
          in here <> nested <> go rest

collectUnusedVariableDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedVariableDiagnostics diagConfig ast =
  case Map.lookup "unused_variable" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name pos = Diagnostic sev "unused_variable" ("unused variable: " <> T.unpack name) (Just pos.spLine) (Just pos.spCol)
          bodies = map (.fnBody) ast.modFunctions <> map (.epBody) ast.modEntries
      in concatMap (unusedVarsInBody mkDiag) bodies
  where
    unusedVarsInBody mkDiag stmts =
      let declaredDecls = collectDecls stmts
          declaredNames = map fst declaredDecls
          (nt1, declaredPairs) = internNamePairs emptyNameTable declaredNames
          declaredWithPos = zipWith (\(name, pos) (_, ident) -> (name, pos, ident)) declaredDecls declaredPairs
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts stmts)
          unused =
            [ (name, pos)
            | (name, pos, ident) <- uniqueByIdWithPos declaredWithPos
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map (uncurry mkDiag) unused

    isIgnored = T.isPrefixOf "_"

    collectDecls = concatMap declsInStmt
    declsInStmt stmt =
      case stmt of
        SLet pos name _ _ -> [(name, pos)]
        SVar pos name _ _ -> [(name, pos)]
        SIf _ _ t e -> collectDecls t <> maybe [] collectDecls e
        SWhile _ _ body -> collectDecls body
        SLoop _ body cont -> collectDecls body <> maybe [] collectDecls cont
        SFor _ initStmt _ contStmt body ->
          maybe [] declsInStmt initStmt <> maybe [] declsInStmt contStmt <> collectDecls body
        SSwitch _ _ cases defBody ->
          concatMap (collectDecls . (.scBody)) cases <> maybe [] collectDecls defBody
        _ -> []

    uniqueByIdWithPos = go IntSet.empty
      where
        go _ [] = []
        go seen ((name, pos, ident):rest)
          | IntSet.member ident seen = go seen rest
          | otherwise = (name, pos, ident) : go (IntSet.insert ident seen) rest

collectUnusedParameterDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectUnusedParameterDiagnostics diagConfig ast =
  case Map.lookup "unused_parameter" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name pos = Diagnostic sev "unused_parameter" ("unused parameter: " <> T.unpack name) (Just pos.spLine) (Just pos.spCol)
      in concatMap (unusedParamsInFunction mkDiag) (ast.modFunctions)
          <> concatMap (unusedParamsInEntry mkDiag) (ast.modEntries)
  where
    unusedParamsInFunction mkDiag fn =
      let paramDecls = map (\p -> (p.paramName, p.paramPos)) fn.fnParams
          params = map fst paramDecls
          (nt1, paramPairs) = internNamePairs emptyNameTable params
          paramWithPos = zipWith (\(name, pos) (_, ident) -> (name, pos, ident)) paramDecls paramPairs
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts fn.fnBody)
          unused =
            [ (name, pos)
            | (name, pos, ident) <- uniqueByIdWithPos paramWithPos
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map (uncurry mkDiag) unused
    unusedParamsInEntry mkDiag entry =
      let paramDecls = map (\p -> (p.paramName, p.paramPos)) entry.epParams
          params = map fst paramDecls
          (nt1, paramPairs) = internNamePairs emptyNameTable params
          paramWithPos = zipWith (\(name, pos) (_, ident) -> (name, pos, ident)) paramDecls paramPairs
          (_, usedIds) = internNameSet nt1 (collectUsesInStmts entry.epBody)
          unused =
            [ (name, pos)
            | (name, pos, ident) <- uniqueByIdWithPos paramWithPos
            , not (IntSet.member ident usedIds)
            , not (isIgnored name)
            ]
      in map (uncurry mkDiag) unused
    isIgnored = T.isPrefixOf "_"

    uniqueByIdWithPos = go IntSet.empty
      where
        go _ [] = []
        go seen ((name, pos, ident):rest)
          | IntSet.member ident seen = go seen rest
          | otherwise = (name, pos, ident) : go (IntSet.insert ident seen) rest

collectShadowingDiagnostics :: DiagnosticConfig -> ModuleAst -> [Diagnostic]
collectShadowingDiagnostics diagConfig ast =
  case Map.lookup "shadowing" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag name pos = Diagnostic sev "shadowing" ("name shadows outer scope: " <> T.unpack name) (Just pos.spLine) (Just pos.spCol)
      in concatMap (shadowingInFunction mkDiag) (ast.modFunctions)
          <> concatMap (shadowingInEntry mkDiag) (ast.modEntries)
  where
    shadowingInFunction mkDiag fn =
      let params = map (.paramName) fn.fnParams
          (nt1, paramIds) = internNames emptyNameTable params
      in shadowingInStmts mkDiag nt1 [IntSet.empty, IntSet.fromList paramIds] fn.fnBody
    shadowingInEntry mkDiag entry =
      let params = map (.paramName) entry.epParams
          (nt1, paramIds) = internNames emptyNameTable params
      in shadowingInStmts mkDiag nt1 [IntSet.empty, IntSet.fromList paramIds] entry.epBody

    shadowingInStmts mkDiag table scopes0 stmts = let (diags, _, _) = go table scopes0 stmts in diags
      where
        go table0 scopesAcc [] = ([], table0, scopesAcc)
        go table0 scopesAcc (stmt:rest) =
          let (diags1, table1, scopes1) = shadowingInStmt mkDiag table0 scopesAcc stmt
              (diags2, table2, scopes2) = go table1 scopes1 rest
          in (diags1 <> diags2, table2, scopes2)

    shadowingInStmt mkDiag table scopes stmt =
      case stmt of
        SLet pos name _ _ ->
          let (ident, table') = internName name table
          in (diagIfShadow mkDiag scopes ident name pos, table', addToCurrent ident scopes)
        SVar pos name _ _ ->
          let (ident, table') = internName name table
          in (diagIfShadow mkDiag scopes ident name pos, table', addToCurrent ident scopes)
        SIf _ _ thenBody elseBody ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes thenBody
              (diags2, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) elseBody
          in (diags1 <> diags2, table2, scopes)
        SWhile _ _ body ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes body
          in (diags1, table1, scopes)
        SLoop _ body continuing ->
          let (diags1, table1) = shadowingInBlock mkDiag table scopes body
              (diags2, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) continuing
          in (diags1 <> diags2, table2, scopes)
        SFor _ initStmt _ contStmt body ->
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
        SSwitch _ _ cases defBody ->
          let (diagsCases, table1) = shadowingInCases mkDiag table scopes cases
              (diagsDef, table2) = maybe ([], table1) (shadowingInBlock mkDiag table1 scopes) defBody
          in (diagsCases <> diagsDef, table2, scopes)
        _ -> ([], table, scopes)

    shadowingInBlock mkDiag table scopes =
      shadowingInStmtsWithTable mkDiag table (pushScope scopes)

    shadowingInStmtsWithTable mkDiag = go
      where
        go table0 _ [] = ([], table0)
        go table0 scopes0 (stmt:rest) =
          let (diags1, table1, scopes1) = shadowingInStmt mkDiag table0 scopes0 stmt
              (diags2, table2) = go table1 scopes1 rest
          in (diags1 <> diags2, table2)

    shadowingInCases mkDiag table scopes =
      foldl' step ([], table)
      where
        step (diagsAcc, tableAcc) sc =
          let (diags, table') = shadowingInBlock mkDiag tableAcc scopes sc.scBody
          in (diagsAcc <> diags, table')

    diagIfShadow mkDiag scopes ident name pos
      | isIgnored name = []
      | otherwise =
          let outers = drop 1 scopes
          in [mkDiag name pos | any (IntSet.member ident) outers]

    addToCurrent ident scopes =
      case scopes of
        [] -> [IntSet.singleton ident]
        current : rest -> IntSet.insert ident current : rest

    pushScope scopes = IntSet.empty : scopes

    isIgnored = T.isPrefixOf "_"

collectConstantConditionDiagnostics :: DiagnosticConfig -> ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ModuleAst -> [Diagnostic]
collectConstantConditionDiagnostics diagConfig ctx constIndex fnIndex structIndex ast =
  case Map.lookup "constant_condition" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag pos msg = Diagnostic sev "constant_condition" msg (Just pos.spLine) (Just pos.spCol)
      in concatMap (constantCondInStmts mkDiag) (map (.fnBody) ast.modFunctions)
          <> concatMap (constantCondInStmts mkDiag . (.epBody)) ast.modEntries
  where
    constantCondInStmts mkDiag = concatMap (constantCondInStmt mkDiag)

    constantCondInStmt mkDiag stmt =
      case stmt of
        SIf _ cond thenBody elseBody ->
          checkCond mkDiag "if" cond
            <> constantCondInStmts mkDiag thenBody
            <> maybe [] (constantCondInStmts mkDiag) elseBody
        SWhile _ cond body ->
          checkCond mkDiag "while" cond <> constantCondInStmts mkDiag body
        SLoop _ body continuing ->
          constantCondInStmts mkDiag body <> maybe [] (constantCondInStmts mkDiag) continuing
        SFor _ initStmt condExpr contStmt body ->
          maybe [] (constantCondInStmt mkDiag) initStmt
            <> maybe [] (constantCondInStmt mkDiag) contStmt
            <> maybe [] (checkCond mkDiag "for") condExpr
            <> constantCondInStmts mkDiag body
        SSwitch _ _ cases defBody ->
          concatMap (constantCondInStmts mkDiag . (.scBody)) cases <> maybe [] (constantCondInStmts mkDiag) defBody
        SBreakIf _ cond -> checkCond mkDiag "break if" cond
        _ -> []

    checkCond mkDiag label cond =
      either
        (const [])
        (const [mkDiag (exprPos cond) ("constant condition in " <> label)])
        (evalConstBoolExpr ctx constIndex fnIndex structIndex cond)

collectDuplicateCaseDiagnostics :: DiagnosticConfig -> ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ModuleAst -> [Diagnostic]
collectDuplicateCaseDiagnostics diagConfig ctx constIndex fnIndex structIndex ast =
  case Map.lookup "duplicate_case" diagConfig of
    Nothing -> []
    Just DiagOff -> []
    Just sev ->
      let mkDiag val pos = Diagnostic sev "duplicate_case" ("duplicate switch case selector: " <> val) (Just pos.spLine) (Just pos.spCol)
      in concatMap (duplicateInStmts mkDiag) (map (.fnBody) ast.modFunctions)
          <> concatMap (duplicateInStmts mkDiag . (.epBody)) ast.modEntries
  where
    duplicateInStmts mkDiag = concatMap (duplicateInStmt mkDiag)

    duplicateInStmt mkDiag stmt =
      case stmt of
        SIf _ _ thenBody elseBody ->
          duplicateInStmts mkDiag thenBody <> maybe [] (duplicateInStmts mkDiag) elseBody
        SWhile _ _ body -> duplicateInStmts mkDiag body
        SLoop _ body continuing ->
          duplicateInStmts mkDiag body <> maybe [] (duplicateInStmts mkDiag) continuing
        SFor _ initStmt _ contStmt body ->
          maybe [] (duplicateInStmt mkDiag) initStmt
            <> maybe [] (duplicateInStmt mkDiag) contStmt
            <> duplicateInStmts mkDiag body
        SSwitch _ _ cases defBody ->
          let selectors = concatMap (.scSelectors) cases
              values = mapMaybe (constSelectorValueWithPos ctx constIndex fnIndex structIndex) selectors
              dupes = duplicateValuesWithPos values
              diags = map (uncurry mkDiag) dupes
              nested = concatMap (duplicateInStmts mkDiag . (.scBody)) cases <> maybe [] (duplicateInStmts mkDiag) defBody
          in diags <> nested
        _ -> []

    constSelectorValueWithPos ctx' constIndex' fnIndex' structIndex' expr =
      either
        (const Nothing)
        (\(ConstInt scalar val) -> Just (renderKey (scalar, val), exprPos expr))
        (evalConstIntExpr ctx' constIndex' fnIndex' structIndex' expr)

    duplicateValuesWithPos vals =
      let (dups, _) =
            foldl'
              (\(acc, seen) (key, pos) ->
                 if Set.member key seen
                   then (acc <> [(key, pos)], seen)
                   else (acc, Set.insert key seen))
              ([], Set.empty)
              vals
      in dups

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
    SLet _ _ _ expr -> collectUsesInExpr expr
    SVar _ _ _ mExpr -> maybe [] collectUsesInExpr mExpr
    SAssign _ lv expr -> collectUsesInLValue lv <> collectUsesInExpr expr
    SAssignOp _ lv _ expr -> collectUsesInLValue lv <> collectUsesInExpr expr
    SInc _ lv -> collectUsesInLValue lv
    SDec _ lv -> collectUsesInLValue lv
    SExpr _ expr -> collectUsesInExpr expr
    SIf _ cond t e -> collectUsesInExpr cond <> collectUsesInStmts t <> maybe [] collectUsesInStmts e
    SWhile _ cond body -> collectUsesInExpr cond <> collectUsesInStmts body
    SLoop _ body cont -> collectUsesInStmts body <> maybe [] collectUsesInStmts cont
    SFor _ initStmt condExpr contStmt body ->
      maybe [] collectUsesInStmt initStmt
        <> maybe [] collectUsesInExpr condExpr
        <> maybe [] collectUsesInStmt contStmt
        <> collectUsesInStmts body
    SSwitch _ expr cases defBody ->
      collectUsesInExpr expr <> concatMap collectUsesInCase cases <> maybe [] collectUsesInStmts defBody
    SBreakIf _ expr -> collectUsesInExpr expr
    SReturn _ mexpr -> maybe [] collectUsesInExpr mexpr
    _ -> []

collectUsesInCase :: SwitchCase -> [Text]
collectUsesInCase sc =
  concatMap collectUsesInExpr sc.scSelectors <> collectUsesInStmts sc.scBody

collectUsesInExpr :: Expr -> [Text]
collectUsesInExpr expr =
  case expr of
    EVar _ name -> [name]
    EInt _ _ -> []
    EFloat _ _ -> []
    EBool _ _ -> []
    EBinary _ _ a b -> collectUsesInExpr a <> collectUsesInExpr b
    EUnary _ _ a -> collectUsesInExpr a
    ECall _ _ args -> concatMap collectUsesInExpr args
    EBitcast _ _ arg -> collectUsesInExpr arg
    EField _ base _ -> collectUsesInExpr base
    EIndex _ base idx -> collectUsesInExpr base <> collectUsesInExpr idx

collectUsesInLValue :: LValue -> [Text]
collectUsesInLValue lv =
  case lv of
    LVVar _ name -> [name]
    LVField _ base _ -> collectUsesInLValue base
    LVIndex _ base idx -> collectUsesInLValue base <> collectUsesInExpr idx
    LVDeref _ expr -> collectUsesInExpr expr

validateStruct :: ModuleContext -> Scope -> StructDecl -> Either CompileError ()
validateStruct ctx scope decl =
  mapM_ (validateType ctx scope . (.fdType)) decl.sdFields

validateBinding :: ModuleContext -> Scope -> BindingDecl -> Either CompileError ()
validateBinding ctx scope decl = validateType ctx scope decl.bdType

validateGlobalVar :: ModuleContext -> Scope -> GlobalVarDecl -> Either CompileError ()
validateGlobalVar ctx scope decl = do
  validateType ctx scope decl.gvType
  mapM_ (validateExpr ctx scope) decl.gvInit
  case decl.gvType of
    TyPtr {} -> Left (CompileError "global pointer types are not supported" Nothing Nothing)
    _ -> Right ()
  case decl.gvSpace of
    "private" -> Right ()
    "workgroup" ->
      case decl.gvInit of
        Nothing -> Right ()
        Just _ -> Left (CompileError "workgroup variables cannot have initializers" Nothing Nothing)
    _ -> Left (CompileError ("unsupported global address space: " <> textToString decl.gvSpace) Nothing Nothing)

validateConst :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Scope -> ConstDecl -> Either CompileError ()
validateConst ctx constIndex fnIndex structIndex scope decl = do
  validateExpr ctx scope decl.cdExpr
  case decl.cdType of
    Nothing -> Right ()
    Just ty -> do
      val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty decl.cdExpr
      _ <- coerceConstValueToType ctx structIndex ty val
      Right ()

validateAlias :: ModuleContext -> Scope -> AliasDecl -> Either CompileError ()
validateAlias ctx scope decl = validateType ctx scope decl.adType

validateOverride :: ModuleContext -> Scope -> OverrideIndex -> OverrideDecl -> Either CompileError ()
validateOverride ctx scope _overrideIndex decl = do
  case decl.odType of
    Nothing -> Left (CompileError "override type could not be inferred" Nothing Nothing)
    Just ty -> validateType ctx scope ty
  case decl.odExpr of
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
  mapM_ (validateType ctx scope . (.paramType)) fn.fnParams
  mapM_ (validateType ctx scope) (maybeToList fn.fnReturnType)
  let paramNames = map (.paramName) fn.fnParams
  let paramNamesWithPos = map (\p -> (p.paramName, p.paramPos)) fn.fnParams
  ensureNoDuplicatesAt "function parameters" paramNamesWithPos
  let scope1 = scopeWithParams scope paramNames
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope1 fn.fnBody

validateEntryPoint :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> EntryPoint -> Either CompileError ()
validateEntryPoint ctx constIndex fnIndex structIndex skipConstEval scope entry = do
  mapM_ (validateType ctx scope . (.paramType)) entry.epParams
  mapM_ (validateType ctx scope) (maybeToList entry.epReturnType)
  let paramNames = map (.paramName) entry.epParams
  let paramNamesWithPos = map (\p -> (p.paramName, p.paramPos)) entry.epParams
  ensureNoDuplicatesAt "entry point parameters" paramNamesWithPos
  let scope1 = scopeWithParams scope paramNames
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval scope1 entry.epBody

validateStmtList :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> [Stmt] -> Either CompileError ()
validateStmtList ctx constIndex fnIndex structIndex skipConstEval = go
  where
    go _ [] = Right ()
    go sc (stmt:rest) = do
      sc' <- validateStmt ctx constIndex fnIndex structIndex skipConstEval sc stmt
      go sc' rest

validateStmt :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> Stmt -> Either CompileError Scope
validateStmt ctx constIndex fnIndex structIndex skipConstEval scope stmt =
  withPos (stmtPos stmt) $
    case stmt of
      SLet pos name mType expr -> do
        mapM_ (validateType ctx scope) mType
        validateExpr ctx scope expr
        scopeAddAt pos scope name
      SVar pos name mType mExpr -> do
        mapM_ (validateType ctx scope) mType
        mapM_ (validateExpr ctx scope) mExpr
        scopeAddAt pos scope name
      SAssign _ lv expr -> validateLValue ctx scope lv >> validateExpr ctx scope expr >> Right scope
      SAssignOp _ lv _ expr -> validateLValue ctx scope lv >> validateExpr ctx scope expr >> Right scope
      SInc _ lv -> validateLValue ctx scope lv >> Right scope
      SDec _ lv -> validateLValue ctx scope lv >> Right scope
      SExpr _ expr -> validateExpr ctx scope expr >> Right scope
      SIf _ cond thenBody elseBody -> do
        validateExpr ctx scope cond
        validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) thenBody
        mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) elseBody
        Right scope
      SWhile _ cond body -> do
        validateExpr ctx scope cond
        validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) body
        Right scope
      SLoop _ body continuing -> do
        validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope) body
        mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) continuing
        Right scope
      SFor _ initStmt condExpr contStmt body -> do
        scope1 <- case initStmt of
          Nothing -> Right scope
          Just s -> validateStmt ctx constIndex fnIndex structIndex skipConstEval scope s
        mapM_ (validateExpr ctx scope1) condExpr
        mapM_ (validateStmt ctx constIndex fnIndex structIndex skipConstEval scope1) contStmt
        validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope1) body
        Right scope
      SSwitch _ expr cases defBody -> do
        validateExpr ctx scope expr
        mapM_ (validateSwitchCase ctx constIndex fnIndex structIndex skipConstEval scope) cases
        mapM_ (validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope)) defBody
        Right scope
      SBreak _ -> Right scope
      SBreakIf _ cond -> validateExpr ctx scope cond >> Right scope
      SContinue _ -> Right scope
      SDiscard _ -> Right scope
      SFallthrough _ ->
        if scope.scAllowFallthrough
          then Right scope
          else Left (CompileError "fallthrough is only allowed in switch cases" Nothing Nothing)
      SReturn _ mexpr -> mapM_ (validateExpr ctx scope) mexpr >> Right scope

validateSwitchCase :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Bool -> Scope -> SwitchCase -> Either CompileError ()
validateSwitchCase ctx constIndex fnIndex structIndex skipConstEval scope sc = do
  _ <- analyzeFallthroughPlacement sc.scBody
  unless skipConstEval $
    mapM_ (evalConstIntExpr ctx constIndex fnIndex structIndex) sc.scSelectors
  let scope' = scope { scAllowFallthrough = True }
  validateStmtList ctx constIndex fnIndex structIndex skipConstEval (enterBlock scope') sc.scBody

analyzeFallthroughPlacement :: [Stmt] -> Either CompileError (Bool, [Stmt])
analyzeFallthroughPlacement body =
  case body of
    [] -> Right (False, [])
    _ ->
      let initStmts = init body
          lastStmt = last body
          nestedFallthrough = any stmtHasFallthrough initStmts
      in case lastStmt of
          SFallthrough pos ->
            if nestedFallthrough
              then Left (errorAtPos pos "fallthrough must be the last statement in a switch case")
              else Right (True, initStmts)
          _ ->
            if nestedFallthrough || stmtHasFallthrough lastStmt
              then Left (errorAtPos (stmtPos lastStmt) "fallthrough must be the last statement in a switch case")
              else Right (False, body)

stmtHasFallthrough :: Stmt -> Bool
stmtHasFallthrough stmt =
  case stmt of
    SFallthrough _ -> True
    SIf _ _ thenBody elseBody ->
      any stmtHasFallthrough thenBody || maybe False (any stmtHasFallthrough) elseBody
    SWhile _ _ body -> any stmtHasFallthrough body
    SLoop _ body continuing ->
      any stmtHasFallthrough body || maybe False (any stmtHasFallthrough) continuing
    SFor _ initStmt _ condStmt body ->
      maybe False stmtHasFallthrough initStmt
        || maybe False stmtHasFallthrough condStmt
        || any stmtHasFallthrough body
    SSwitch {} -> False
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
    CVArray elemTy vals -> TyArray elemTy (ArrayLenFixed (length vals))
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
    TyArray elemTy (ArrayLenFixed len) ->
      case val of
        CVArray _ elems | length elems == len -> do
          elems' <- mapM (coerceConstValueToType ctx structIndex elemTy) elems
          Right (CVArray elemTy elems')
        _ -> Left (CompileError "array constant does not match type" Nothing Nothing)
    TyArray _ ArrayLenRuntime ->
      Left (CompileError "runtime array constants are not supported" Nothing Nothing)
    TyArray _ (ArrayLenExpr _) ->
      Left (CompileError "array constant requires a fixed length" Nothing Nothing)
    TyStructRef name ->
      case val of
        CVStruct structName pairs | structName == name -> do
          decl <- resolveStructDecl ctx structIndex name
          fields' <- mapM (coerceStructField ctx structIndex pairs) decl.sdFields
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

defaultConstValue :: ModuleContext -> StructIndex -> Type -> Either CompileError ConstValue
defaultConstValue ctx structIndex ty =
  case ty of
    TyScalar I32 -> Right (CVInt (ConstInt I32 0))
    TyScalar U32 -> Right (CVInt (ConstInt U32 0))
    TyScalar F32 -> Right (CVFloat (ConstFloat F32 0.0))
    TyScalar F16 -> Right (CVFloat (ConstFloat F16 0.0))
    TyScalar Bool -> Right (CVBool False)
    TyVector n scalar -> do
      let scalarVal =
            case scalar of
              I32 -> CVInt (ConstInt I32 0)
              U32 -> CVInt (ConstInt U32 0)
              F32 -> CVFloat (ConstFloat F32 0.0)
              F16 -> CVFloat (ConstFloat F16 0.0)
              Bool -> CVBool False
      Right (CVVector n scalar (replicate n scalarVal))
    TyMatrix cols rows scalar -> do
      let scalarVal =
            case scalar of
              I32 -> CVInt (ConstInt I32 0)
              U32 -> CVInt (ConstInt U32 0)
              F32 -> CVFloat (ConstFloat F32 0.0)
              F16 -> CVFloat (ConstFloat F16 0.0)
              Bool -> CVBool False
          col = CVVector rows scalar (replicate rows scalarVal)
      Right (CVMatrix cols rows scalar (replicate cols col))
    TyArray elemTy (ArrayLenFixed n) -> do
      elems <- replicateM n (defaultConstValue ctx structIndex elemTy)
      Right (CVArray elemTy elems)
    TyArray _ ArrayLenRuntime ->
      Left (CompileError "runtime array constants are not supported" Nothing Nothing)
    TyArray _ (ArrayLenExpr _) ->
      Left (CompileError "array constants require a fixed length" Nothing Nothing)
    TyStructRef name -> do
      decl <- case lookupStructIndex structIndex [] name of
        Nothing -> Left (CompileError ("unknown struct: " <> T.unpack name) Nothing Nothing)
        Just d -> Right d
      fields <- mapM (\f -> (f.fdName,) <$> defaultConstValue ctx structIndex f.fdType) decl.sdFields
      Right (CVStruct name fields)
    TySampler -> Left (CompileError "sampler types are not supported for local variables" Nothing Nothing)
    TySamplerComparison -> Left (CompileError "sampler types are not supported for local variables" Nothing Nothing)
    TyTexture1D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTexture1DArray {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTexture2D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTexture2DArray {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTexture3D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureCube {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureCubeArray {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureMultisampled2D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureDepth2D -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureDepth2DArray -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureDepthCube -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureDepthCubeArray -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyTextureDepthMultisampled2D -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyStorageTexture1D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyStorageTexture2D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyStorageTexture2DArray {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyStorageTexture3D {} -> Left (CompileError "texture types are not supported for local variables" Nothing Nothing)
    TyAtomic {} -> Left (CompileError "atomic types are not supported for local variables" Nothing Nothing)
    TyPtr {} -> Left (CompileError "pointer types are not supported for local variables" Nothing Nothing)

coerceStructField :: ModuleContext -> StructIndex -> [(Text, ConstValue)] -> FieldDecl -> Either CompileError (Text, ConstValue)
coerceStructField ctx structIndex pairs field = do
  val' <- case lookup field.fdName pairs of
    Just v -> Right v
    Nothing -> Left (CompileError ("missing field: " <> textToString field.fdName) Nothing Nothing)
  coerced <- coerceConstValueToType ctx structIndex field.fdType val'
  Right (field.fdName, coerced)

resolveStructDecl :: ModuleContext -> StructIndex -> Text -> Either CompileError StructDecl
resolveStructDecl ctx structIndex name =
  case splitQName name of
    [] -> Left (CompileError "invalid struct reference" Nothing Nothing)
    [single] ->
      case lookupStruct ctx.mcPath single of
        Just decl -> Right decl
        Nothing ->
          case Map.lookup single ctx.mcItemAliases of
            Just target ->
              case splitLast target of
                Nothing -> Left (CompileError "invalid struct reference" Nothing Nothing)
                Just (path, item) ->
                  case lookupStruct path item of
                    Just decl -> Right decl
                    Nothing -> Left (CompileError ("unknown struct: " <> textToString item) Nothing Nothing)
            Nothing -> Left (CompileError ("unknown struct: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 ctx.mcModuleAliases of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid struct reference" Nothing Nothing)
            Just (path, item) ->
              case lookupStruct path item of
                Just decl -> Right decl
                Nothing -> Left (CompileError ("unknown struct: " <> textToString item) Nothing Nothing)
  where
    lookupStruct = lookupStructIndex structIndex

evalConstValueWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstValue
evalConstValueWithEnv ctx constIndex fnIndex structIndex env = go
  where
    go seen fnSeen ex =
      case ex of
        EUnary _ OpAddr inner -> evalConstAddressOf seen fnSeen inner
        EUnary _ OpDeref inner -> do
          ptr <- go seen fnSeen inner
          derefConstPointer seen fnSeen ptr
        EVar _ name ->
          case Map.lookup name env of
            Just envBinding -> Right envBinding.cbValue
            Nothing -> do
              (path, ident) <- resolveConstRef ctx name
              let key = T.intercalate "::" (path <> [ident])
              when (Set.member key seen) $
                Left (CompileError "cycle detected while evaluating constant expression" Nothing Nothing)
              let entry = lookupConstIndex constIndex path ident
              case entry of
                Nothing -> Left (CompileError ("unknown constant: " <> textToString ident) Nothing Nothing)
                Just (mTy, expr') -> do
                  val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
                  case mTy of
                    Nothing -> Right val
                    Just ty -> coerceConstValueToType ctx structIndex ty val
        EField _ base field -> do
          baseVal <- go seen fnSeen base
          evalConstFieldAccess baseVal field
        EIndex _ base idxExpr -> do
          baseVal <- go seen fnSeen base
          idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen idxExpr
          evalConstIndexAccess baseVal idxVal
        ECall _ name args ->
          case parseVectorCtorName name of
            Just (n, targetScalar) -> evalConstVectorCtor n targetScalar seen fnSeen args
            Nothing ->
              case name of
                "array" -> evalConstArrayCtor seen fnSeen args
                _ ->
                  case parseMatrixCtorName name of
                    Just (cols, rows, targetScalar) ->
                      evalConstMatrixCtor cols rows targetScalar seen fnSeen args
                    Nothing ->
                      if isBuiltinName name
                        then evalConstScalarValue seen fnSeen ex
                        else
                          case resolveStructDecl ctx structIndex name of
                            Right decl -> evalConstStructCtor decl.sdName decl.sdFields seen fnSeen args
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

    evalConstVectorCtor n targetScalar seen fnSeen args = do
      when (null args) $
        Left (CompileError "vector constructor needs arguments" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      let singleScalar =
            case vals of
              [v] -> isScalarConst v
              _ -> False
      flattened <- fmap concat (mapM flattenArg vals)
      case flattened of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        (firstVal : _) -> do
          scalar <- case targetScalar of
            Just s -> Right s
            Nothing -> constScalarType firstVal
          coerced <- mapM (coerceConstScalarValue scalar) flattened
          let filled =
                case (singleScalar, coerced) of
                  (True, [v]) -> replicate n v
                  _ -> coerced
          when (length filled /= n) $
            Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
          Right (CVVector n scalar filled)
      where
        isScalarConst v =
          case v of
            CVInt _ -> True
            CVFloat _ -> True
            CVBool _ -> True
            _ -> False
        flattenArg v =
          case v of
            CVInt _ -> Right [v]
            CVFloat _ -> Right [v]
            CVBool _ -> Right [v]
            CVVector _ _ comps -> Right comps
            _ -> Left (CompileError "vector constructor arguments must be scalars or vectors" Nothing Nothing)

    evalConstMatrixCtor cols rows targetScalar seen fnSeen args = do
      when (null args) $
        Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
      vals <- mapM (go seen fnSeen) args
      let scalarCount = cols * rows
      case vals of
        [v] | isScalarConst v -> do
          scalar <- case targetScalar of
            Just s -> Right s
            Nothing -> constScalarType v
          v' <- coerceConstScalarValue scalar v
          let zero = constZeroScalar scalar
          let colsVals =
                [ CVVector rows scalar [if rowIx == colIx then v' else zero | rowIx <- [0 .. rows - 1]]
                | colIx <- [0 .. cols - 1]
                ]
          Right (CVMatrix cols rows scalar colsVals)
        _ -> do
          flattened <- fmap concat (mapM (flattenMatrixArg rows) vals)
          case flattened of
            [] -> Left (CompileError "matrix constructor needs arguments" Nothing Nothing)
            (firstVal:_) -> do
              scalar <- case targetScalar of
                Just s -> Right s
                Nothing -> constScalarType firstVal
              scalars <- mapM (coerceConstScalarValue scalar) flattened
              when (length scalars /= scalarCount) $
                Left (CompileError "matrix constructor expects column vectors or a full scalar list" Nothing Nothing)
              let colsVals = map (CVVector rows scalar) (chunk rows scalars)
              Right (CVMatrix cols rows scalar colsVals)
      where
        isScalarConst v =
          case v of
            CVInt _ -> True
            CVFloat _ -> True
            CVBool _ -> True
            _ -> False
        flattenMatrixArg rows' v =
          case v of
            CVInt _ -> Right [v]
            CVFloat _ -> Right [v]
            CVBool _ -> Right [v]
            CVVector n _ comps
              | n == rows' -> Right comps
              | otherwise -> Left (CompileError "matrix constructor expects column vectors matching the row count" Nothing Nothing)
            _ -> Left (CompileError "matrix constructor arguments must be scalars or vectors" Nothing Nothing)
        constZeroScalar s =
          case s of
            I32 -> CVInt (ConstInt I32 0)
            U32 -> CVInt (ConstInt U32 0)
            F32 -> CVFloat (ConstFloat F32 0)
            F16 -> CVFloat (ConstFloat F16 0)
            Bool -> CVBool False
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
      let pairs = zip (map (.fdName) fields) vals
      Right (CVStruct name pairs)

    evalConstAddressOf seen fnSeen inner =
      case exprToLValue inner of
        Nothing -> Left (CompileError "address-of requires an lvalue" Nothing Nothing)
        Just lv ->
          case lv of
            LVDeref _ ptrExpr -> do
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
        LVVar pos name -> Right (LVVar pos name)
        LVField pos base field -> LVField pos <$> freezeConstLValue seen fnSeen base <*> pure field
        LVIndex pos base idxExpr -> do
          base' <- freezeConstLValue seen fnSeen base
          ConstInt _ idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen idxExpr
          Right (LVIndex pos base' (EInt pos idxVal))
        LVDeref _ _ ->
          Left (CompileError "address-of requires an lvalue" Nothing Nothing)

evalConstFunctionValueWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> FunctionDecl -> Either CompileError ConstValue
evalConstFunctionValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns decl = do
  (_, ctrl, _) <- evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns constEvalMaxSteps decl.fnBody
  val <- case ctrl of
    CCReturn v -> Right v
    CCNone -> Left (CompileError "const function must return a value" Nothing Nothing)
    CCBreak -> Left (CompileError "break used outside of a loop" Nothing Nothing)
    CCContinue -> Left (CompileError "continue used outside of a loop" Nothing Nothing)
  case decl.fnReturnType of
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
        let params = decl.fnParams
        when (length params /= length argVals) $
          Left (CompileError "const function overload not found" Nothing Nothing)
        coercedArgs <- zipWithM (coerceConstValueToType ctx structIndex . (.paramType)) params argVals
        let bindings = [ConstBinding val False | val <- coercedArgs]
        let env' = Map.fromList (zip (map (.paramName) params) bindings)
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
evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns = go env
  where
    go env' fuel' [] = Right (env', CCNone, fuel')
    go env' fuel' (stmt:rest) = do
      (envNext, ctrl, fuelNext) <- evalConstStmt ctx constIndex fnIndex structIndex env' seenConsts seenFns fuel' stmt
      case ctrl of
        CCNone -> go envNext fuelNext rest
        _ -> Right (envNext, ctrl, fuelNext)

evalConstStmt :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Int -> Stmt -> Either CompileError (ConstEnv, ConstControl, Int)
evalConstStmt ctx constIndex fnIndex structIndex env seenConsts seenFns fuel stmt =
  withPos (stmtPos stmt) $ do
    fuel' <- consumeConstFuel fuel
    case stmt of
      SLet _ name mType expr -> do
        val0 <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
        val <- case mType of
          Nothing -> Right val0
          Just ty -> coerceConstValueToType ctx structIndex ty val0
        Right (Map.insert name (ConstBinding val False) env, CCNone, fuel')
      SVar _ name mType mExpr -> do
        val0 <- case mExpr of
          Just expr -> evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
          Nothing ->
            case mType of
              Nothing -> Left (CompileError "var declaration requires a type or initializer" Nothing Nothing)
              Just ty -> defaultConstValue ctx structIndex ty
        val <- case mType of
          Nothing -> Right val0
          Just ty -> coerceConstValueToType ctx structIndex ty val0
        Right (Map.insert name (ConstBinding val True) env, CCNone, fuel')
      SAssign _ lv expr -> do
        oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
        newVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
        let targetTy = constValueType oldVal
        newVal' <- coerceConstValueToType ctx structIndex targetTy newVal
        env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal'
        Right (env', CCNone, fuel')
      SAssignOp _ lv op expr -> do
        oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
        rhsVal <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
        let targetTy = constValueType oldVal
        rhsVal' <- coerceConstValueToType ctx structIndex targetTy rhsVal
        newVal <- evalConstAssignOp op oldVal rhsVal'
        env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
        Right (env', CCNone, fuel')
      SInc _ lv -> do
        oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
        one <- constScalarOne oldVal
        newVal <- evalConstAssignOp OpAdd oldVal one
        env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
        Right (env', CCNone, fuel')
      SDec _ lv -> do
        oldVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns lv
        one <- constScalarOne oldVal
        newVal <- evalConstAssignOp OpSub oldVal one
        env' <- evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal
        Right (env', CCNone, fuel')
      SExpr _ expr -> do
        _ <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
        Right (env, CCNone, fuel')
      SSwitch _ expr cases defBody -> do
        cases' <- expandSwitchCases cases defBody
        ConstInt _ selector <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
        matchCase selector cases' defBody fuel'
      SIf _ cond thenBody elseBody -> do
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
      SWhile _ cond body -> evalConstWhile cond body fuel'
      SLoop _ body continuing -> evalConstLoop body continuing fuel'
      SFor _ initStmt condExpr contStmt body -> evalConstFor initStmt condExpr contStmt body fuel'
      SBreak _ -> Right (env, CCBreak, fuel')
      SBreakIf _ cond -> do
        ok <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns cond
        Right (env, if ok then CCBreak else CCNone, fuel')
      SContinue _ -> Right (env, CCContinue, fuel')
      SReturn _ (Just expr) -> do
        val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
        Right (env, CCReturn val, fuel')
      SReturn _ Nothing ->
        Left (CompileError "const function return requires a value" Nothing Nothing)
      _ ->
        Left (CompileError "const function bodies may only contain let/var, if, switch, loops, assignments, expr, and return statements" Nothing Nothing)
  where
    matchCase _ [] defBody fuel'' =
      case defBody of
        Nothing -> Right (env, CCNone, fuel'')
        Just body -> evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' body
    matchCase selector (case0:rest) defBody fuel'' = do
      matched <- or <$> mapM (matchesSelector selector) case0.scSelectors
      if matched
        then evalConstStmtList ctx constIndex fnIndex structIndex env seenConsts seenFns fuel'' case0.scBody
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
    LVVar _ name ->
      case Map.lookup name env of
        Just envBinding -> Right envBinding.cbValue
        Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
    LVField _ base field -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      evalConstFieldAccess baseVal field
    LVIndex _ base idxExpr -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns idxExpr
      evalConstIndexAccess baseVal idxVal
    LVDeref _ expr -> do
      ptr <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns expr
      case ptr of
        CVPointer _ ptrLv -> evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns ptrLv
        _ -> Left (CompileError "deref requires a pointer value" Nothing Nothing)

evalConstLValueSet :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> LValue -> ConstValue -> Either CompileError ConstEnv
evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns lv newVal =
  case lv of
    LVVar _ name ->
        case Map.lookup name env of
          Nothing -> Left (CompileError ("unknown variable: " <> textToString name) Nothing Nothing)
          Just envBinding ->
            if envBinding.cbMutable
              then Right (Map.insert name envBinding { cbValue = newVal } env)
              else Left (CompileError "cannot assign to immutable let binding" Nothing Nothing)
    LVField _ base field -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      updated <- updateFieldValue baseVal field newVal
      evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns base updated
    LVIndex _ base idxExpr -> do
      baseVal <- evalConstLValueGet ctx constIndex fnIndex structIndex env seenConsts seenFns base
      idxVal <- evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env seenConsts seenFns idxExpr
      updated <- updateIndexValue baseVal idxVal newVal
      evalConstLValueSet ctx constIndex fnIndex structIndex env seenConsts seenFns base updated
    LVDeref _ expr -> do
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
evalConstIntExpr ctx constIndex fnIndex structIndex =
  evalConstIntExprWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty

evalConstIntExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError ConstInt
evalConstIntExprWithEnv ctx constIndex fnIndex structIndex env = go
  where
    go seen fnSeen ex =
      case ex of
        EInt _ n -> do
          (scalar, val) <- selectIntLiteralScalar n
          case scalar of
            I32 -> do
              checkI32 val
              Right (ConstInt I32 val)
            U32 -> do
              checkU32 val
              Right (ConstInt U32 val)
            _ -> Left (CompileError "integer literal must be i32 or u32" Nothing Nothing)
        EUnary _ OpNeg inner -> do
          ConstInt scalar val <- go seen fnSeen inner
          case scalar of
            I32 -> do
              let v = negate val
              checkI32 v
              Right (ConstInt I32 v)
            U32 -> Left (CompileError "unary minus is not supported for u32 in const expressions" Nothing Nothing)
            _ -> Left (CompileError "unary minus expects integer constants" Nothing Nothing)
        EUnary _ OpDeref _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToInt val
        EFloat _ _ ->
          Left (CompileError "const int expression references a float literal" Nothing Nothing)
        ECall _ "u32" [arg] ->
          case go seen fnSeen arg of
            Right (ConstInt _ n) -> do
              checkU32 n
              Right (ConstInt U32 n)
            _ -> do
              ConstFloat _ v <- evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen arg
              let n = truncate v :: Integer
              checkU32 n
              Right (ConstInt U32 n)
        ECall _ "i32" [arg] ->
          case go seen fnSeen arg of
            Right (ConstInt _ n) -> do
              checkI32 n
              Right (ConstInt I32 n)
            _ -> do
              ConstFloat _ v <- evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen arg
              let n = truncate v :: Integer
              checkI32 n
              Right (ConstInt I32 n)
        EBitcast _ _ _ ->
          Left (CompileError "bitcast is not allowed in const integer expressions" Nothing Nothing)
        ECall _ name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToInt val
        EBinary _ op a b -> do
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
        EVar _ name ->
          case Map.lookup name env of
            Just envBinding ->
              case envBinding.cbValue of
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
                Just (mTy, expr') -> do
                  val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
                  val' <- case mTy of
                    Nothing -> Right val
                    Just ty -> coerceConstValueToType ctx structIndex ty val
                  constValueToInt val'
        EField _ _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToInt val
        EIndex _ _ _ -> do
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
evalConstFloatExprWithEnv ctx constIndex fnIndex structIndex env = go
  where
    go seen fnSeen ex =
      case ex of
        EFloat _ f -> Right (ConstFloat F32 (realToFrac f))
        EInt _ n -> do
          (_, val) <- selectIntLiteralScalar n
          Right (ConstFloat F32 (fromIntegral val))
        EUnary _ OpNeg inner -> do
          cf <- go seen fnSeen inner
          Right (applyFloatOp cf.cfScalar negate cf.cfValue)
        EUnary _ OpDeref _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        ECall _ "f32" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (convertConstFloatTo F32 cf)
        ECall _ "f16" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (convertConstFloatTo F16 cf)
        ECall _ "abs" [arg] -> do
          cf <- evalFloatArg seen fnSeen arg
          Right (applyFloatOp cf.cfScalar abs cf.cfValue)
        ECall _ "min" [a, b] -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          Right (applyFloatOp scalar id (min x y))
        ECall _ "max" [a, b] -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          Right (applyFloatOp scalar id (max x y))
        ECall _ "clamp" [xExpr, loExpr, hiExpr] -> do
          cfX <- evalFloatArg seen fnSeen xExpr
          cfLo <- evalFloatArg seen fnSeen loExpr
          cfHi <- evalFloatArg seen fnSeen hiExpr
          let (scalar1, x, lo) = coerceFloatPair cfX cfLo
          let ConstFloat _ hi = convertConstFloatTo scalar1 cfHi
          Right (applyFloatOp scalar1 id (min (max x lo) hi))
        ECall _ "mix" [aExpr, bExpr, tExpr] -> do
          cfA <- evalFloatArg seen fnSeen aExpr
          cfB <- evalFloatArg seen fnSeen bExpr
          cfT <- evalFloatArg seen fnSeen tExpr
          let (scalar1, a, b) = coerceFloatPair cfA cfB
          let ConstFloat _ t = convertConstFloatTo scalar1 cfT
          Right (applyFloatOp scalar1 id (a * (1.0 - t) + b * t))
        ECall _ "select" [aExpr, bExpr, condExpr] -> do
          cond <- evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env seen fnSeen condExpr
          cfA <- evalFloatArg seen fnSeen aExpr
          cfB <- evalFloatArg seen fnSeen bExpr
          Right (if cond then cfB else cfA)
        ECall _ name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToFloat val
        EBinary _ op a b -> do
          cfA <- evalFloatArg seen fnSeen a
          cfB <- evalFloatArg seen fnSeen b
          let (scalar, x, y) = coerceFloatPair cfA cfB
          case op of
            OpAdd -> Right (applyFloatOp scalar id (x + y))
            OpSub -> Right (applyFloatOp scalar id (x - y))
            OpMul -> Right (applyFloatOp scalar id (x * y))
            OpDiv ->
              if y == 0.0
                then Left (CompileError "division by zero in constant expression" Nothing Nothing)
                else Right (applyFloatOp scalar id (x / y))
            _ -> Left (CompileError "unsupported const float operation" Nothing Nothing)
        EVar _ name ->
          case Map.lookup name env of
            Just envBinding ->
              case envBinding.cbValue of
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
                Just (mTy, expr') -> do
                  val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
                  val' <- case mTy of
                    Nothing -> Right val
                    Just ty -> coerceConstValueToType ctx structIndex ty val
                  constValueToFloat val'
        EField _ _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        EIndex _ _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToFloat val
        _ -> Left (CompileError "const float expression requires a constant float expression" Nothing Nothing)

    evalFloatArg seen fnSeen ex = do
      cf <- go seen fnSeen ex
      Right cf

    coerceFloatPair a b =
      let scalar = if a.cfScalar == F32 || b.cfScalar == F32 then F32 else F16
          a' = convertConstFloatTo scalar a
          b' = convertConstFloatTo scalar b
      in (scalar, a'.cfValue, b'.cfValue)

    applyFloatOp scalar f v =
      let v' = f v
      in ConstFloat scalar (if scalar == F16 then quantizeF16 v' else v')

evalConstBoolExpr :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> Expr -> Either CompileError Bool
evalConstBoolExpr ctx constIndex fnIndex structIndex =
  evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex Map.empty Set.empty Set.empty

evalConstBoolExprWithEnv :: ModuleContext -> ConstIndex -> FunctionIndex -> StructIndex -> ConstEnv -> Set.Set Text -> Set.Set Text -> Expr -> Either CompileError Bool
evalConstBoolExprWithEnv ctx constIndex fnIndex structIndex env = go
  where
    go seen fnSeen ex =
      case ex of
        EBool _ b -> Right b
        EUnary _ OpNot a -> not <$> go seen fnSeen a
        EUnary _ OpDeref _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToBool val
        EBinary _ OpAnd a b -> (&&) <$> go seen fnSeen a <*> go seen fnSeen b
        EBinary _ OpOr a b -> (||) <$> go seen fnSeen a <*> go seen fnSeen b
        EBinary _ OpEq a b -> evalEq seen fnSeen a b
        EBinary _ OpNe a b -> not <$> evalEq seen fnSeen a b
        EBinary _ OpLt a b -> evalCmp seen fnSeen (<) (<) a b
        EBinary _ OpLe a b -> evalCmp seen fnSeen (<=) (<=) a b
        EBinary _ OpGt a b -> evalCmp seen fnSeen (>) (>) a b
        EBinary _ OpGe a b -> evalCmp seen fnSeen (>=) (>=) a b
        ECall _ "select" [aExpr, bExpr, condExpr] -> do
          cond <- go seen fnSeen condExpr
          if cond then go seen fnSeen bExpr else go seen fnSeen aExpr
        EBitcast _ _ _ ->
          Left (CompileError "bitcast is not allowed in const boolean expressions" Nothing Nothing)
        ECall _ name args
          | not (isBuiltinName name) -> do
              val <- evalConstUserFunctionCall ctx constIndex fnIndex structIndex env seen fnSeen name args
              constValueToBool val
        EVar _ name ->
          case Map.lookup name env of
            Just envBinding ->
              case envBinding.cbValue of
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
                Just (mTy, expr') -> do
                  val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env (Set.insert key seen) fnSeen expr'
                  val' <- case mTy of
                    Nothing -> Right val
                    Just ty -> coerceConstValueToType ctx structIndex ty val
                  constValueToBool val'
        EField _ _ _ -> do
          val <- evalConstValueWithEnv ctx constIndex fnIndex structIndex env seen fnSeen ex
          constValueToBool val
        EIndex _ _ _ -> do
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
                  CVVector {} -> True
                  CVMatrix {} -> True
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
      if Set.member single ctx.mcConstNames
        then Right (ctx.mcPath, single)
        else case Map.lookup single ctx.mcItemAliases of
          Just target ->
            case splitLast target of
              Nothing -> Left (CompileError "invalid constant reference" Nothing Nothing)
              Just (path, item) -> Right (path, item)
          Nothing -> Left (CompileError ("unknown constant: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 ctx.mcModuleAliases of
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
      if Set.member single ctx.mcFunctionNames
        then Right (ctx.mcPath, single)
        else case Map.lookup single ctx.mcItemAliases of
          Just target ->
            case splitLast target of
              Nothing -> Left (CompileError "invalid function reference" Nothing Nothing)
              Just (path, item) -> Right (path, item)
          Nothing -> Left (CompileError ("unknown function: " <> textToString single) Nothing Nothing)
    seg0 : segRest ->
      case Map.lookup seg0 ctx.mcModuleAliases of
        Nothing -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> Left (CompileError "invalid function reference" Nothing Nothing)
            Just (path, item) -> Right (path, item)

validateLValue :: ModuleContext -> Scope -> LValue -> Either CompileError ()
validateLValue ctx scope lv =
  withPos (lvaluePos lv) $
    case lv of
      LVVar _ name -> validateName ctx scope name
      LVField _ base _ -> validateLValue ctx scope base
      LVIndex _ base idx -> validateLValue ctx scope base >> validateExpr ctx scope idx
      LVDeref _ expr -> validateExpr ctx scope expr

validateExpr :: ModuleContext -> Scope -> Expr -> Either CompileError ()
validateExpr ctx scope expr =
  withPos (exprPos expr) $
    case expr of
      EVar _ name -> validateName ctx scope name
      EInt _ _ -> Right ()
      EFloat _ _ -> Right ()
      EBool _ _ -> Right ()
      EBinary _ _ a b -> validateExpr ctx scope a >> validateExpr ctx scope b
      EUnary _ OpAddr a ->
        case exprToLValue a of
          Nothing -> Left (CompileError "address-of requires an addressable expression" Nothing Nothing)
          Just _ -> validateExpr ctx scope a
      EUnary _ OpDeref a -> validateExpr ctx scope a
      EUnary _ _ a -> validateExpr ctx scope a
      ECall _ name args -> validateName ctx scope name >> mapM_ (validateExpr ctx scope) args
      EBitcast _ ty arg -> validateType ctx scope ty >> validateExpr ctx scope arg
      EField _ base _ -> validateExpr ctx scope base
      EIndex _ base idx -> validateExpr ctx scope base >> validateExpr ctx scope idx

validateType :: ModuleContext -> Scope -> Type -> Either CompileError ()
validateType ctx scope ty =
  case ty of
    TyStructRef name ->
      case lookupNameId scope name of
        Just sid | IntSet.member sid scope.scTypeAliases -> Right ()
        _ -> validateName ctx scope name
    TyArray elemTy _ -> validateType ctx scope elemTy
    TyVector _ _ -> Right ()
    TyMatrix {} -> Right ()
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
                  || IntSet.member sid scope.scGlobals
                  || IntSet.member sid scope.scItemAliases -> Right ()
            _ -> Left (CompileError ("unknown identifier: " <> textToString single) Nothing Nothing)
    seg0 : _ ->
      case lookupNameId scope seg0 of
        Just sid | IntSet.member sid scope.scModuleAliases -> Right ()
        _ -> Left (CompileError ("unknown module alias: " <> textToString seg0) Nothing Nothing)

lookupNameId :: Scope -> Text -> Maybe Int
lookupNameId scope name = Map.lookup name scope.scNameTable.ntMap

scopeLocals :: Scope -> IntSet.IntSet
scopeLocals scope = foldl' IntSet.union IntSet.empty scope.scScopes

currentScope :: Scope -> IntSet.IntSet
currentScope scope =
  case scope.scScopes of
    [] -> IntSet.empty
    s : _ -> s

scopeAdd :: Scope -> Text -> Either CompileError Scope
scopeAdd scope name =
  let (nameId, table') = internName name scope.scNameTable
      scope' = scope { scNameTable = table' }
  in if IntSet.member nameId (currentScope scope')
      then Left (CompileError ("duplicate local declaration: " <> textToString name) Nothing Nothing)
      else
        if not scope'.scAllowShadowing
          && not (T.isPrefixOf "_" name)
          && IntSet.member nameId (scopeOuterLocals scope')
          then Left (CompileError ("shadowing is not allowed: " <> textToString name) Nothing Nothing)
          else
            case scope'.scScopes of
              [] ->
                Right scope' { scScopes = [IntSet.singleton nameId] }
              current : rest ->
                Right scope' { scScopes = IntSet.insert nameId current : rest }

scopeOuterLocals :: Scope -> IntSet.IntSet
scopeOuterLocals scope =
  case scope.scScopes of
    [] -> IntSet.empty
    _ : rest -> foldl' IntSet.union IntSet.empty rest

enterBlock :: Scope -> Scope
enterBlock scope = scope { scScopes = IntSet.empty : scope.scScopes }

scopeWithParams :: Scope -> [Text] -> Scope
scopeWithParams scope names =
  let (table', ids) = internNames scope.scNameTable names
  in scope { scNameTable = table', scScopes = [IntSet.fromList ids] }

scopeAddAt :: SrcPos -> Scope -> Text -> Either CompileError Scope
scopeAddAt pos scope name =
  withPos pos (scopeAdd scope name)

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

ensureNoDuplicatesAt :: Text -> [(Text, SrcPos)] -> Either CompileError ()
ensureNoDuplicatesAt label names =
  go Set.empty names
  where
    go _ [] = Right ()
    go seen ((name, pos):rest) =
      if Set.member name seen
        then Left (errorAtPos pos ("duplicate " <> textToString label <> ": " <> T.unpack name))
        else go (Set.insert name seen) rest

isBuiltinName :: Text -> Bool
isBuiltinName name =
  name `Set.member` builtinNames
    || isJust (parseVectorCtorName name)
    || isJust (parseMatrixCtorName name)

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
  let ctx = fromMaybe (buildModuleContext rootPath "" node) (Map.lookup node.mnFile ctxs)
      ast = node.mnAst
      prefix = ctx.mcPath
      rename = qualNameWithRoot ctx.mcRootPath prefix
      renameDecl = rename
      rewriteTy = rewriteType ctx
      rewriteParam = rewriteParamType ctx
      rewriteField = rewriteFieldDecl ctx
      rewriteStmt = rewriteStmtNames ctx
      rewriteExpr = rewriteExprNames ctx
      rewriteFn fn =
        fn
          { fnName = renameDecl fn.fnName
          , fnParams = map rewriteParam fn.fnParams
          , fnReturnType = fmap rewriteTy fn.fnReturnType
          , fnBody = map rewriteStmt fn.fnBody
          }
      rewriteConst c =
        c
          { cdName = renameDecl c.cdName
          , cdType = c.cdType
          , cdExpr = rewriteExpr c.cdExpr
          }
      rewriteAlias a =
        a
          { adName = renameDecl a.adName
          , adType = rewriteTy a.adType
          }
      rewriteOverride o =
        o
          { odName = renameDecl o.odName
          , odType = fmap rewriteTy o.odType
          , odExpr = fmap rewriteExpr o.odExpr
          }
      rewriteBinding b =
        b
          { bdName = renameDecl b.bdName
          , bdType = rewriteTy b.bdType
          }
      rewriteGlobal g =
        g
          { gvName = renameDecl g.gvName
          , gvType = rewriteTy g.gvType
          , gvInit = fmap rewriteExpr g.gvInit
          }
      rewriteStruct s =
        s
          { sdName = renameDecl s.sdName
          , sdFields = map rewriteField s.sdFields
          }
      rewriteConstAssert (ConstAssert pos expr) =
        ConstAssert pos (rewriteExpr expr)
      entries =
        if prefix == rootPath
          then
            [ e
                { epParams = map rewriteParam e.epParams
                , epReturnType = fmap rewriteTy e.epReturnType
                , epBody = map rewriteStmt e.epBody
                }
            | e <- ast.modEntries
            ]
          else []
      ast' =
        ast
          { modImports = []
          , modAliases = map rewriteAlias (ast.modAliases)
          , modStructs = map rewriteStruct (ast.modStructs)
          , modBindings = map rewriteBinding (ast.modBindings)
          , modGlobals = map rewriteGlobal (ast.modGlobals)
          , modConsts = map rewriteConst (ast.modConsts)
          , modOverrides = map rewriteOverride (ast.modOverrides)
          , modConstAsserts = map rewriteConstAssert (ast.modConstAsserts)
          , modFunctions = map rewriteFn (ast.modFunctions)
          , modEntries = entries
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
      if Set.member single ctx.mcLocals
        then
          if ctx.mcPath == ctx.mcRootPath
            then single
            else qualNameWithRoot ctx.mcRootPath ctx.mcPath single
        else
          case Map.lookup single ctx.mcItemAliases of
            Just target ->
              case splitLast target of
                Nothing -> single
                Just (path, item) -> qualNameWithRoot ctx.mcRootPath path item
            Nothing -> single
    seg0 : segRest ->
      case Map.lookup seg0 ctx.mcModuleAliases of
        Just target ->
          case splitLast (target <> segRest) of
            Nothing -> name
            Just (path, item) -> qualNameWithRoot ctx.mcRootPath path item
        Nothing ->
          case splitLast (seg0 : segRest) of
            Nothing -> name
            Just (path, item) -> qualNameWithRoot ctx.mcRootPath path item

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
  param { paramType = rewriteType ctx param.paramType }

rewriteFieldDecl :: ModuleContext -> FieldDecl -> FieldDecl
rewriteFieldDecl ctx field =
  field { fdType = rewriteType ctx field.fdType }

rewriteStmtNames :: ModuleContext -> Stmt -> Stmt
rewriteStmtNames ctx stmt =
  case stmt of
    SLet pos name mType expr -> SLet pos name mType (rewriteExprNames ctx expr)
    SVar pos name mType mExpr -> SVar pos name mType (fmap (rewriteExprNames ctx) mExpr)
    SAssign pos lv expr -> SAssign pos (rewriteLValueNames ctx lv) (rewriteExprNames ctx expr)
    SAssignOp pos lv op expr -> SAssignOp pos (rewriteLValueNames ctx lv) op (rewriteExprNames ctx expr)
    SInc pos lv -> SInc pos (rewriteLValueNames ctx lv)
    SDec pos lv -> SDec pos (rewriteLValueNames ctx lv)
    SExpr pos expr -> SExpr pos (rewriteExprNames ctx expr)
    SIf pos cond thenBody elseBody ->
      SIf pos (rewriteExprNames ctx cond)
          (map (rewriteStmtNames ctx) thenBody)
          (fmap (map (rewriteStmtNames ctx)) elseBody)
    SWhile pos cond body ->
      SWhile pos (rewriteExprNames ctx cond) (map (rewriteStmtNames ctx) body)
    SLoop pos body continuing ->
      SLoop pos (map (rewriteStmtNames ctx) body) (fmap (map (rewriteStmtNames ctx)) continuing)
    SFor pos initStmt condExpr contStmt body ->
      SFor pos (fmap (rewriteStmtNames ctx) initStmt)
           (fmap (rewriteExprNames ctx) condExpr)
           (fmap (rewriteStmtNames ctx) contStmt)
           (map (rewriteStmtNames ctx) body)
    SSwitch pos expr cases defBody ->
      SSwitch pos (rewriteExprNames ctx expr)
        (map rewriteCase cases)
        (fmap (map (rewriteStmtNames ctx)) defBody)
    SBreak pos -> SBreak pos
    SBreakIf pos cond -> SBreakIf pos (rewriteExprNames ctx cond)
    SContinue pos -> SContinue pos
    SDiscard pos -> SDiscard pos
    SFallthrough pos -> SFallthrough pos
    SReturn pos expr -> SReturn pos (fmap (rewriteExprNames ctx) expr)
  where
    rewriteCase sc =
      sc { scSelectors = map (rewriteExprNames ctx) sc.scSelectors
         , scBody = map (rewriteStmtNames ctx) sc.scBody
         }

rewriteLValueNames :: ModuleContext -> LValue -> LValue
rewriteLValueNames ctx lv =
  case lv of
    LVVar pos name -> LVVar pos (rewriteIdent ctx name)
    LVField pos base field -> LVField pos (rewriteLValueNames ctx base) field
    LVIndex pos base idx -> LVIndex pos (rewriteLValueNames ctx base) (rewriteExprNames ctx idx)
    LVDeref pos expr -> LVDeref pos (rewriteExprNames ctx expr)

rewriteExprNames :: ModuleContext -> Expr -> Expr
rewriteExprNames ctx expr =
  case expr of
    EVar pos name -> EVar pos (rewriteIdent ctx name)
    EInt {} -> expr
    EFloat {} -> expr
    EBool {} -> expr
    EBinary pos op a b -> EBinary pos op (rewriteExprNames ctx a) (rewriteExprNames ctx b)
    EUnary pos op a -> EUnary pos op (rewriteExprNames ctx a)
    ECall pos name args -> ECall pos (rewriteIdent ctx name) (map (rewriteExprNames ctx) args)
    EBitcast pos ty arg -> EBitcast pos (rewriteType ctx ty) (rewriteExprNames ctx arg)
    EField pos base field -> EField pos (rewriteExprNames ctx base) field
    EIndex pos base idx -> EIndex pos (rewriteExprNames ctx base) (rewriteExprNames ctx idx)

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
