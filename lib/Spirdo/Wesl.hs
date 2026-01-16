{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Spirdo.Wesl
  ( -- * Compile-time interface types
    BindingKind(..)
  , ScalarType(..)
  , Field(..)
  , Ty(..)
  , Binding(..)
  , BindingDesc(..)
  , ReflectBindings(..)
  , samplerBindings
  , uniformBindings
  , samplerBindingsFor
  , uniformBindingsFor
  , CompiledShader(..)
  , SomeCompiledShader(..)
  , ShaderInterface(..)
  , BindingInfo(..)
  , TypeLayout(..)
  , FieldLayout(..)
  , CompileError(..)
  , CompileOptions(..)
  , defaultCompileOptions
  , compileWeslToSpirv
  , compileWeslToSpirvWith
  , compileWeslToSpirvBytes
  , compileWeslToSpirvBytesWith
  , wesl
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.List (mapAccumL)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Word (Word8, Word16, Word32)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import GHC.Float (castFloatToWord32)
import Language.Haskell.TH (Exp, Q)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

-- Type-level interface representation

data BindingKind = BUniform | BStorageRead | BStorageReadWrite | BSampler | BTexture2D
  deriving (Eq, Show)

data ScalarType = SI32 | SU32 | SF32 | SBool
  deriving (Eq, Show)

data Field = Field Symbol Ty

data Ty
  = TScalar ScalarType
  | TVec Nat ScalarType
  | TArray Nat Ty
  | TRuntimeArray Ty
  | TStruct [Field]
  | TSampler
  | TTexture2D ScalarType

-- | Type-level binding descriptor: name, kind, set, binding, type.
data Binding = Binding Symbol BindingKind Nat Nat Ty

data BindingDesc = BindingDesc
  { descName :: String
  , descKind :: BindingKind
  , descGroup :: Word32
  , descBinding :: Word32
  } deriving (Eq, Show)

class KnownBindingKind (k :: BindingKind) where
  bindingKindVal :: Proxy k -> BindingKind

instance KnownBindingKind 'BUniform where
  bindingKindVal _ = BUniform

instance KnownBindingKind 'BStorageRead where
  bindingKindVal _ = BStorageRead

instance KnownBindingKind 'BStorageReadWrite where
  bindingKindVal _ = BStorageReadWrite

instance KnownBindingKind 'BSampler where
  bindingKindVal _ = BSampler

instance KnownBindingKind 'BTexture2D where
  bindingKindVal _ = BTexture2D

class ReflectBinding (b :: Binding) where
  reflectBinding :: Proxy b -> BindingDesc

instance (KnownSymbol name, KnownBindingKind kind, KnownNat set, KnownNat binding) => ReflectBinding ('Binding name kind set binding ty) where
  reflectBinding _ =
    BindingDesc
      { descName = symbolVal (Proxy @name)
      , descKind = bindingKindVal (Proxy @kind)
      , descGroup = fromIntegral (natVal (Proxy @set))
      , descBinding = fromIntegral (natVal (Proxy @binding))
      }

class ReflectBindings (iface :: [Binding]) where
  reflectBindings :: Proxy iface -> [BindingDesc]

instance ReflectBindings '[] where
  reflectBindings _ = []

instance (ReflectBinding b, ReflectBindings bs) => ReflectBindings (b ': bs) where
  reflectBindings _ = reflectBinding (Proxy @b) : reflectBindings (Proxy @bs)

samplerBindings :: forall iface. ReflectBindings iface => Proxy iface -> [BindingDesc]
samplerBindings _ =
  filter (\b -> descKind b == BSampler || descKind b == BTexture2D) (reflectBindings (Proxy @iface))

uniformBindings :: forall iface. ReflectBindings iface => Proxy iface -> [BindingDesc]
uniformBindings _ =
  filter (\b -> descKind b == BUniform) (reflectBindings (Proxy @iface))

samplerBindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> [BindingDesc]
samplerBindingsFor _ = samplerBindings (Proxy @iface)

uniformBindingsFor :: forall iface. ReflectBindings iface => CompiledShader iface -> [BindingDesc]
uniformBindingsFor _ = uniformBindings (Proxy @iface)

-- Runtime interface representation

data Scalar = I32 | U32 | F32 | Bool
  deriving (Eq, Show)

data Type
  = TyScalar Scalar
  | TyVector Int Scalar
  | TyArray Type (Maybe Int)
  | TyStructRef String
  | TySampler
  | TyTexture2D Scalar
  deriving (Eq, Show)

data FieldDecl = FieldDecl
  { fdName :: String
  , fdType :: Type
  } deriving (Eq, Show)

data StructDecl = StructDecl
  { sdName :: String
  , sdFields :: [FieldDecl]
  } deriving (Eq, Show)

data BindingInfo = BindingInfo
  { biName :: String
  , biKind :: BindingKind
  , biGroup :: Word32
  , biBinding :: Word32
  , biType :: TypeLayout
  } deriving (Eq, Show)

data FieldLayout = FieldLayout
  { flName :: String
  , flOffset :: Word32
  , flType :: TypeLayout
  } deriving (Eq, Show)

data TypeLayout
  = TLScalar Scalar Word32 Word32
  | TLVector Int Scalar Word32 Word32
  | TLArray (Maybe Int) Word32 TypeLayout Word32 Word32
  | TLStruct String [FieldLayout] Word32 Word32
  | TLSampler
  | TLTexture2D Scalar
  deriving (Eq, Show)

layoutAlign :: TypeLayout -> Word32
layoutAlign tl = case tl of
  TLScalar _ a _ -> a
  TLVector _ _ a _ -> a
  TLArray _ _ _ a _ -> a
  TLStruct _ _ a _ -> a
  TLSampler -> 0
  TLTexture2D _ -> 0

layoutSize :: TypeLayout -> Word32
layoutSize tl = case tl of
  TLScalar _ _ s -> s
  TLVector _ _ _ s -> s
  TLArray _ _ _ _ s -> s
  TLStruct _ _ _ s -> s
  TLSampler -> 0
  TLTexture2D _ -> 0

vectorLayout :: Int -> (Word32, Word32)
vectorLayout n = case n of
  2 -> (8, 8)
  3 -> (16, 16)
  4 -> (16, 16)
  _ -> (16, 16)

newtype ShaderInterface = ShaderInterface
  { siBindings :: [BindingInfo]
  } deriving (Eq, Show)

-- | Compiled shader with a type-level interface description.
data CompiledShader (iface :: [Binding]) = CompiledShader
  { shaderSpirv :: ByteString
  , shaderInterface :: ShaderInterface
  }

-- | Existential wrapper for runtime compilation.
data SomeCompiledShader = forall iface. SomeCompiledShader (CompiledShader iface)

-- Errors and options

data CompileError = CompileError
  { ceMessage :: String
  , ceLine :: Maybe Int
  , ceColumn :: Maybe Int
  } deriving (Eq, Show)

newtype CompileOptions = CompileOptions
  { spirvVersion :: Word32
  }

-- Default to SPIR-V 1.6 (0x00010600). Override if needed.
defaultCompileOptions :: CompileOptions
defaultCompileOptions = CompileOptions 0x00010600

-- Public API

compileWeslToSpirv :: String -> Either CompileError SomeCompiledShader
compileWeslToSpirv = compileWeslToSpirvWith defaultCompileOptions

compileWeslToSpirvWith :: CompileOptions -> String -> Either CompileError SomeCompiledShader
compileWeslToSpirvWith opts src = do
  moduleAst <- parseModule src
  iface <- buildInterface moduleAst
  spirv <- emitSpirv opts moduleAst iface
  let shader = CompiledShader spirv iface
  pure (SomeCompiledShader shader)

compileWeslToSpirvBytes :: String -> Either CompileError ByteString
compileWeslToSpirvBytes src = compileWeslToSpirvBytesWith defaultCompileOptions src

compileWeslToSpirvBytesWith :: CompileOptions -> String -> Either CompileError ByteString
compileWeslToSpirvBytesWith opts src = do
  moduleAst <- parseModule src
  iface <- buildInterface moduleAst
  emitSpirv opts moduleAst iface

-- Quasiquoter

wesl :: QuasiQuoter
wesl =
  QuasiQuoter
    { quoteExp = weslExp
    , quotePat = const (fail "wesl: pattern context not supported")
    , quoteType = const (fail "wesl: type context not supported")
    , quoteDec = const (fail "wesl: declaration context not supported")
    }

weslExp :: String -> Q Exp
weslExp src =
  case compileWeslToSpirvWith defaultCompileOptions src of
    Left err -> fail (renderError err)
    Right (SomeCompiledShader (CompiledShader bytes iface)) -> do
      bytesExp <- bytesToExp bytes
      ifaceExp <- interfaceToExp iface
      let ifaceTy = interfaceToType iface
      let shaderExp = TH.AppE (TH.AppE (TH.ConE 'CompiledShader) bytesExp) ifaceExp
      pure (TH.SigE shaderExp (TH.AppT (TH.ConT ''CompiledShader) ifaceTy))

-- Parsing

data SrcPos = SrcPos
  { spLine :: Int
  , spCol :: Int
  } deriving (Eq, Show)

data Token = Token
  { tkKind :: TokKind
  , tkPos :: SrcPos
  } deriving (Eq, Show)

data TokKind
  = TkIdent String
  | TkInt Integer
  | TkFloat Float
  | TkString String
  | TkSymbol String
  deriving (Eq, Show)

data ModuleAst = ModuleAst
  { modStructs :: [StructDecl]
  , modBindings :: [BindingDecl]
  , modEntry :: Maybe EntryPoint
  } deriving (Eq, Show)

data BindingDecl = BindingDecl
  { bdName :: String
  , bdKind :: BindingKind
  , bdGroup :: Word32
  , bdBinding :: Word32
  , bdType :: Type
  } deriving (Eq, Show)

data Stage = StageCompute | StageFragment | StageVertex
  deriving (Eq, Show)

data Param = Param
  { paramName :: String
  , paramType :: Type
  , paramAttrs :: [Attr]
  } deriving (Eq, Show)

data EntryPoint = EntryPoint
  { epName :: String
  , epStage :: Stage
  , epWorkgroupSize :: Maybe (Word32, Word32, Word32)
  , epParams :: [Param]
  , epReturnType :: Maybe Type
  , epReturnLocation :: Maybe Word32
  , epReturnBuiltin :: Maybe String
  , epBody :: [Stmt]
  } deriving (Eq, Show)

data Stmt
  = SLet String Expr
  | SVar String Expr
  | SAssign LValue Expr
  | SReturn (Maybe Expr)
  deriving (Eq, Show)

data LValue
  = LVVar String
  | LVField LValue String
  | LVIndex LValue Expr
  deriving (Eq, Show)

data Expr
  = EVar String
  | EInt Integer
  | EFloat Float
  | EBinary BinOp Expr Expr
  | EUnary UnaryOp Expr
  | ECall String [Expr]
  | EField Expr String
  | EIndex Expr Expr
  deriving (Eq, Show)

data BinOp = OpAdd | OpSub | OpMul | OpDiv
  deriving (Eq, Show)

data UnaryOp = OpNeg
  deriving (Eq, Show)

parseModule :: String -> Either CompileError ModuleAst
parseModule src = do
  toks <- lexWesl src
  parseModuleTokens toks

lexWesl :: String -> Either CompileError [Token]
lexWesl = go (SrcPos 1 1)
  where
    go _ [] = pure []
    go pos (c:cs)
      | c == ' ' || c == '\t' = go (advance pos c) cs
      | c == '\n' = go (advance pos c) cs
      | c == '\r' = go (advance pos c) cs
      | c == '/' && prefix "//" (c:cs) =
          let (_, rest, pos') = consumeLine (advance (advance pos '/') '/') (drop 1 cs)
          in go pos' rest
      | c == '/' && prefix "/*" (c:cs) =
          case consumeBlockComment (advance (advance pos '/') '*') (drop 1 cs) of
            Left err -> Left err
            Right (rest, pos') -> go pos' rest
      | isAlpha c || c == '_' =
          let (ident, rest, pos') = consumeIdent pos (c:cs)
          in (Token (TkIdent ident) pos :) <$> go pos' rest
      | isDigit c =
          let (tok, rest, pos') = consumeNumber pos (c:cs)
          in (Token tok pos :) <$> go pos' rest
      | c == '"' =
          case consumeString pos cs of
            Left err -> Left err
            Right (str, rest, pos') -> (Token (TkString str) pos :) <$> go pos' rest
      | c `elem` symbolChars =
          let sym = [c]
          in (Token (TkSymbol sym) pos :) <$> go (advance pos c) cs
      | otherwise =
          Left (CompileError ("unexpected character: " <> [c]) (Just (spLine pos)) (Just (spCol pos)))

    symbolChars = "@:{}();,<>=[]+-*/."

    prefix s xs = take (length s) xs == s

    consumeLine p rest =
      let (line, rest') = break (== '\n') rest
          pos' = foldl' advance p line
      in case rest' of
           [] -> (line, [], pos')
           (_:rs) -> (line, rs, advance pos' '\n')

    consumeBlockComment p cs =
      let goBlock pos' [] = Left (CompileError "unterminated block comment" (Just (spLine pos')) (Just (spCol pos')))
          goBlock pos' (x:y:rest)
            | x == '*' && y == '/' = Right (rest, advance (advance pos' x) y)
            | otherwise = goBlock (advance pos' x) (y:rest)
          goBlock pos' [x] = goBlock (advance pos' x) []
      in goBlock p cs

    consumeIdent p cs =
      let (ident, rest) = span (\x -> isAlphaNum x || x == '_') cs
          pos' = foldl' advance p ident
      in (ident, rest, pos')

    consumeNumber p cs =
      let (digits, rest) = span isDigit cs
      in case rest of
          ('.':r@(d:_)) | isDigit d ->
            let (frac, rest') = span isDigit r
                numStr = digits <> "." <> frac
                pos' = foldl' advance p (digits <> "." <> frac)
            in (TkFloat (read numStr), rest', pos')
          _ ->
            let pos' = foldl' advance p digits
            in (TkInt (read digits), rest, pos')

    consumeString p cs =
      let goStr _ pos' [] = Left (CompileError "unterminated string" (Just (spLine pos')) (Just (spCol pos')))
          goStr acc pos' (x:xs)
            | x == '"' = Right (reverse acc, xs, advance pos' x)
            | x == '\\' = case xs of
                [] -> Left (CompileError "unterminated escape" (Just (spLine pos')) (Just (spCol pos')))
                (e:rest) -> goStr (e:acc) (advance (advance pos' x) e) rest
            | otherwise = goStr (x:acc) (advance pos' x) xs
      in goStr [] (advance p '"') cs

    advance (SrcPos l c) ch
      | ch == '\n' = SrcPos (l + 1) 1
      | otherwise = SrcPos l (c + 1)

parseModuleTokens :: [Token] -> Either CompileError ModuleAst
parseModuleTokens toks =
  let loop accStructs accBindings accEntry rest =
        case rest of
          [] ->
            Right (ModuleAst (reverse accStructs) (reverse accBindings) accEntry)
          _ -> do
            (attrs, rest1) <- parseAttributes rest
            case rest1 of
              (Token (TkIdent "struct") _ : _) -> do
                (structDecl, rest2) <- parseStruct rest1
                loop (structDecl:accStructs) accBindings accEntry rest2
              (Token (TkIdent "var") _ : _) -> do
                (bindingDecl, rest2) <- parseBinding attrs rest1
                loop accStructs (bindingDecl:accBindings) accEntry rest2
              (Token (TkIdent "fn") _ : _) -> do
                (entry, rest2) <- parseFunction attrs rest1
                case accEntry of
                  Nothing -> loop accStructs accBindings (Just entry) rest2
                  Just _ -> Left (errorAt rest1 "multiple entry points are not supported")
              (Token (TkIdent "import") _ : _) ->
                Left (errorAt rest1 "import is not supported yet")
              _ -> Left (errorAt rest1 "expected struct, var, or fn")
  in loop [] [] Nothing toks

parseAttributes :: [Token] -> Either CompileError ([Attr], [Token])
parseAttributes toks =
  let go acc rest =
        case rest of
          (Token (TkSymbol "@") _ : Token (TkIdent name) _ : more) -> do
            (args, rest') <- parseAttrArgs more
            go (Attr name args : acc) rest'
          _ -> Right (reverse acc, rest)
  in go [] toks

parseAttrArgs :: [Token] -> Either CompileError ([AttrArg], [Token])
parseAttrArgs toks =
  case toks of
    (Token (TkSymbol "(") _ : rest) -> parseAttrArgList [] rest
    _ -> Right ([], toks)
  where
    parseAttrArgList acc rest =
      case rest of
        (Token (TkSymbol ")") _ : more) -> Right (reverse acc, more)
        _ -> do
          (arg, rest1) <- parseAttrArg rest
          case rest1 of
            (Token (TkSymbol ",") _ : more) -> parseAttrArgList (arg:acc) more
            (Token (TkSymbol ")") _ : more) -> Right (reverse (arg:acc), more)
            _ -> Left (errorAt rest1 "expected ',' or ')' in attribute arguments")

    parseAttrArg rest =
      case rest of
        (Token (TkInt n) _ : more) -> Right (AttrInt n, more)
        (Token (TkIdent name) _ : more) -> Right (AttrIdent name, more)
        _ -> Left (errorAt rest "expected attribute argument")

parseStruct :: [Token] -> Either CompileError (StructDecl, [Token])
parseStruct toks =
  case toks of
    (Token (TkIdent "struct") _ : Token (TkIdent name) _ : Token (TkSymbol "{") _ : rest) -> do
      (fields, rest1) <- parseStructFields [] rest
      case rest1 of
        (Token (TkSymbol ";") _ : more) -> Right (StructDecl name fields, more)
        _ -> Right (StructDecl name fields, rest1)
    _ -> Left (errorAt toks "malformed struct declaration")

parseStructFields :: [FieldDecl] -> [Token] -> Either CompileError ([FieldDecl], [Token])
parseStructFields acc rest =
  case rest of
    (Token (TkSymbol "}") _ : more) -> Right (reverse acc, more)
    _ -> do
      (field, rest1) <- parseStructField rest
      let rest2 = case rest1 of
            (Token (TkSymbol ",") _ : more) -> more
            (Token (TkSymbol ";") _ : more) -> more
            _ -> rest1
      parseStructFields (field:acc) rest2

parseStructField :: [Token] -> Either CompileError (FieldDecl, [Token])
parseStructField toks =
  case toks of
    (Token (TkIdent name) _ : Token (TkSymbol ":") _ : rest) -> do
      (ty, rest1) <- parseType rest
      Right (FieldDecl name ty, rest1)
    _ -> Left (errorAt toks "expected struct field")

parseBinding :: [Attr] -> [Token] -> Either CompileError (BindingDecl, [Token])
parseBinding attrs toks =
  case toks of
    (Token (TkIdent "var") _ : rest) -> do
      (addrSpace, access, rest1) <- parseAddressSpaceMaybe rest
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      rest5 <- expectSymbol ";" rest4
      (group, binding) <- bindingNumbers attrs
      kind <- case addrSpace of
        Just space -> toBindingKind space access
        Nothing -> bindingKindFromType ty
      Right (BindingDecl name kind group binding ty, rest5)
    _ -> Left (errorAt toks "expected var declaration")

parseAddressSpaceMaybe :: [Token] -> Either CompileError (Maybe String, Maybe String, [Token])
parseAddressSpaceMaybe toks =
  case toks of
    (Token (TkSymbol "<") _ : _) -> do
      (addr, access, rest) <- parseAddressSpace toks
      Right (Just addr, access, rest)
    _ -> Right (Nothing, Nothing, toks)

parseAddressSpace :: [Token] -> Either CompileError (String, Maybe String, [Token])
parseAddressSpace toks =
  case toks of
    (Token (TkSymbol "<") _ : Token (TkIdent addr) _ : rest) ->
      case rest of
        (Token (TkSymbol ",") _ : Token (TkIdent access) _ : Token (TkSymbol ">") _ : more) ->
          Right (addr, Just access, more)
        (Token (TkSymbol ">") _ : more) -> Right (addr, Nothing, more)
        _ -> Left (errorAt rest "expected address space qualifier")
    _ -> Left (errorAt toks "expected address space qualifiers")

parseFunction :: [Attr] -> [Token] -> Either CompileError (EntryPoint, [Token])
parseFunction attrs toks =
  case toks of
    (Token (TkIdent "fn") _ : Token (TkIdent name) _ : Token (TkSymbol "(") _ : rest) -> do
      (stage, workgroup) <- entryAttributes attrs
      (params, rest1) <- parseParams rest
      rest2 <- expectSymbol ")" rest1
      (rest3, retType, retLoc, retBuiltin) <- parseReturnType rest2
      (body, rest4) <- parseBody rest3
      let entry = EntryPoint name stage workgroup params retType retLoc retBuiltin body
      validateEntry entry toks
      Right (entry, rest4)
    _ -> Left (errorAt toks "expected fn declaration")

parseParams :: [Token] -> Either CompileError ([Param], [Token])
parseParams toks =
  case toks of
    (Token (TkSymbol ")") _ : _) -> Right ([], toks)
    _ -> parseParamList [] toks
  where
    parseParamList acc rest = do
      (param, rest1) <- parseParam rest
      case rest1 of
        (Token (TkSymbol ",") _ : more) -> parseParamList (param:acc) more
        _ -> Right (reverse (param:acc), rest1)

    parseParam rest = do
      (attrs, rest1) <- parseAttributes rest
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      pure (Param name ty attrs, rest4)

parseReturnType :: [Token] -> Either CompileError ([Token], Maybe Type, Maybe Word32, Maybe String)
parseReturnType toks =
  case toks of
    (Token (TkSymbol "-") _ : Token (TkSymbol ">") _ : rest) -> do
      (retAttrs, rest1) <- parseAttributes rest
      case rest1 of
        (Token (TkIdent "void") _ : more) -> Right (more, Nothing, attrIntMaybe "location" retAttrs, attrBuiltin retAttrs)
        _ -> do
          (ty, rest2) <- parseType rest1
          Right (rest2, Just ty, attrIntMaybe "location" retAttrs, attrBuiltin retAttrs)
    _ -> Right (toks, Nothing, Nothing, Nothing)

parseBody :: [Token] -> Either CompileError ([Stmt], [Token])
parseBody toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseStatements [] rest
    _ -> Left (errorAt toks "expected function body")

parseStatements :: [Stmt] -> [Token] -> Either CompileError ([Stmt], [Token])
parseStatements acc toks =
  case toks of
    (Token (TkSymbol "}") _ : more) -> Right (reverse acc, more)
    _ -> do
      (stmt, rest) <- parseStmt toks
      parseStatements (stmt:acc) rest

parseStmt :: [Token] -> Either CompileError (Stmt, [Token])
parseStmt toks =
  case toks of
    (Token (TkIdent "let") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      rest4 <- expectSymbol ";" rest3
      Right (SLet name expr, rest4)
    (Token (TkIdent "var") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      rest4 <- expectSymbol ";" rest3
      Right (SVar name expr, rest4)
    (Token (TkIdent "return") _ : rest) ->
      case rest of
        (Token (TkSymbol ";") _ : more) -> Right (SReturn Nothing, more)
        _ -> do
          (expr, rest1) <- parseExpr rest
          rest2 <- expectSymbol ";" rest1
          Right (SReturn (Just expr), rest2)
    _ -> do
      (lv, rest1) <- parseLValue toks
      rest2 <- expectSymbol "=" rest1
      (expr, rest3) <- parseExpr rest2
      rest4 <- expectSymbol ";" rest3
      Right (SAssign lv expr, rest4)

parseLValue :: [Token] -> Either CompileError (LValue, [Token])
parseLValue toks = do
  (name, rest) <- parseIdent toks
  parseLValueTail (LVVar name) rest

parseLValueTail :: LValue -> [Token] -> Either CompileError (LValue, [Token])
parseLValueTail lv toks =
  case toks of
    (Token (TkSymbol ".") _ : Token (TkIdent field) _ : rest) ->
      parseLValueTail (LVField lv field) rest
    (Token (TkSymbol "[") _ : rest) -> do
      (idx, rest1) <- parseExpr rest
      rest2 <- expectSymbol "]" rest1
      parseLValueTail (LVIndex lv idx) rest2
    _ -> Right (lv, toks)

parseExpr :: [Token] -> Either CompileError (Expr, [Token])
parseExpr = parseAddSub

parseAddSub :: [Token] -> Either CompileError (Expr, [Token])
parseAddSub toks = do
  (lhs, rest) <- parseMulDiv toks
  parseAddSubTail lhs rest
  where
    parseAddSubTail lhs toks' =
      case toks' of
        (Token (TkSymbol "+") _ : rest) -> do
          (rhs, rest1) <- parseMulDiv rest
          parseAddSubTail (EBinary OpAdd lhs rhs) rest1
        (Token (TkSymbol "-") _ : rest) -> do
          (rhs, rest1) <- parseMulDiv rest
          parseAddSubTail (EBinary OpSub lhs rhs) rest1
        _ -> Right (lhs, toks')

parseMulDiv :: [Token] -> Either CompileError (Expr, [Token])
parseMulDiv toks = do
  (lhs, rest) <- parseUnaryExpr toks
  parseMulDivTail lhs rest
  where
    parseMulDivTail lhs toks' =
      case toks' of
        (Token (TkSymbol "*") _ : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary OpMul lhs rhs) rest1
        (Token (TkSymbol "/") _ : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary OpDiv lhs rhs) rest1
        _ -> Right (lhs, toks')

parseUnaryExpr :: [Token] -> Either CompileError (Expr, [Token])
parseUnaryExpr toks =
  case toks of
    (Token (TkSymbol "-") _ : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary OpNeg expr, rest1)
    _ -> parsePostfixExpr toks

parsePostfixExpr :: [Token] -> Either CompileError (Expr, [Token])
parsePostfixExpr toks = do
  (base, rest) <- parsePrimaryExpr toks
  parsePostfixTail base rest
  where
    parsePostfixTail expr toks' =
      case toks' of
        (Token (TkSymbol ".") _ : Token (TkIdent field) _ : rest) ->
          parsePostfixTail (EField expr field) rest
        (Token (TkSymbol "[") _ : rest) -> do
          (idx, rest1) <- parseExpr rest
          rest2 <- expectSymbol "]" rest1
          parsePostfixTail (EIndex expr idx) rest2
        _ -> Right (expr, toks')

parsePrimaryExpr :: [Token] -> Either CompileError (Expr, [Token])
parsePrimaryExpr toks =
  case toks of
    (Token (TkInt n) _ : rest) -> Right (EInt n, rest)
    (Token (TkFloat f) _ : rest) -> Right (EFloat f, rest)
    (Token (TkIdent name) _ : rest) ->
      case rest of
        (Token (TkSymbol "(") _ : more) -> do
          (args, rest1) <- parseCallArgs [] more
          Right (ECall name args, rest1)
        _ -> Right (EVar name, rest)
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected expression")

parseCallArgs :: [Expr] -> [Token] -> Either CompileError ([Expr], [Token])
parseCallArgs acc toks =
  case toks of
    (Token (TkSymbol ")") _ : rest) -> Right (reverse acc, rest)
    _ -> do
      (expr, rest1) <- parseExpr toks
      case rest1 of
        (Token (TkSymbol ",") _ : rest2) -> parseCallArgs (expr:acc) rest2
        (Token (TkSymbol ")") _ : rest2) -> Right (reverse (expr:acc), rest2)
        _ -> Left (errorAt rest1 "expected ',' or ')' in call arguments")

validateEntry :: EntryPoint -> [Token] -> Either CompileError ()
validateEntry entry toks =
  case epStage entry of
    StageCompute -> do
      case epWorkgroupSize entry of
        Nothing -> Left (errorAt toks "@workgroup_size is required for @compute")
        Just _ -> pure ()
      case epParams entry of
        [Param _ ty attrs]
          | hasBuiltinGlobalId attrs && isVec3U32 ty -> pure ()
          | otherwise -> Left (errorAt toks "only @builtin(global_invocation_id) vec3<u32> parameter is supported")
        [] -> Left (errorAt toks "@compute entry points require @builtin(global_invocation_id) parameter")
        _ -> Left (errorAt toks "multiple parameters are not supported for @compute")
      case epReturnType entry of
        Nothing -> pure ()
        Just _ -> Left (errorAt toks "compute entry points must return void")
    StageFragment -> do
      ensureFragmentParams (epParams entry) toks
      case epReturnType entry of
        Nothing -> Left (errorAt toks "fragment entry points must return a value")
        Just _ -> pure ()
      case epReturnBuiltin entry of
        Nothing -> pure ()
        Just _ -> Left (errorAt toks "fragment entry points do not support @builtin return values yet")
    StageVertex -> do
      case epWorkgroupSize entry of
        Nothing -> pure ()
        Just _ -> Left (errorAt toks "@workgroup_size is not allowed for @vertex")
      case epParams entry of
        [Param _ ty attrs]
          | paramBuiltin attrs == Just "vertex_index" && isU32 ty -> pure ()
          | otherwise -> Left (errorAt toks "only @builtin(vertex_index) u32 parameter is supported")
        [] -> pure ()
        _ -> Left (errorAt toks "multiple parameters are not supported for @vertex")
      case epReturnType entry of
        Nothing -> Left (errorAt toks "vertex entry points must return a value")
        Just ty ->
          if isVec4F32 ty
            then pure ()
            else Left (errorAt toks "vertex entry points must return vec4<f32>")
      case epReturnBuiltin entry of
        Just "position" -> pure ()
        _ -> Left (errorAt toks "vertex entry points must return @builtin(position)")
      case epReturnLocation entry of
        Nothing -> pure ()
        Just _ -> Left (errorAt toks "vertex entry points do not support @location return values")

ensureFragmentParams :: [Param] -> [Token] -> Either CompileError ()
ensureFragmentParams params toks = do
  let locations = mapMaybe (paramLocation . paramAttrs) params
  if length locations /= length params
    then Left (errorAt toks "fragment parameters must use @location")
    else if hasDuplicates locations
      then Left (errorAt toks "duplicate @location values on fragment parameters")
      else pure ()

paramLocation :: [Attr] -> Maybe Word32
paramLocation = attrIntMaybe "location"

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

parseType :: [Token] -> Either CompileError (Type, [Token])
parseType toks =
  case toks of
    (Token (TkIdent name) _ : rest)
      | name `elem` ["i32", "u32", "f32", "bool"] ->
          Right (TyScalar (parseScalar name), rest)
      | name == "sampler" ->
          Right (TySampler, rest)
      | name == "texture_2d" -> do
          rest1 <- expectSymbol "<" rest
          (inner, rest2) <- parseType rest1
          rest3 <- expectSymbol ">" rest2
          case inner of
            TyScalar scalar -> Right (TyTexture2D scalar, rest3)
            _ -> Left (errorAt rest "texture_2d element must be a scalar")
      | name `elem` ["vec2", "vec3", "vec4"] -> do
          rest1 <- expectSymbol "<" rest
          (inner, rest2) <- parseType rest1
          rest3 <- expectSymbol ">" rest2
          case inner of
            TyScalar scalar -> Right (TyVector (vecSize name) scalar, rest3)
            _ -> Left (errorAt rest "vector element must be a scalar")
      | name == "array" -> do
          rest1 <- expectSymbol "<" rest
          (elemTy, rest2) <- parseType rest1
          case rest2 of
            (Token (TkSymbol ",") _ : Token (TkInt n) _ : rest3) -> do
              rest4 <- expectSymbol ">" rest3
              Right (TyArray elemTy (Just (fromIntegral n)), rest4)
            _ -> do
              rest3 <- expectSymbol ">" rest2
              Right (TyArray elemTy Nothing, rest3)
      | otherwise -> Right (TyStructRef name, rest)
    _ -> Left (errorAt toks "expected type")

parseIdent :: [Token] -> Either CompileError (String, [Token])
parseIdent toks =
  case toks of
    (Token (TkIdent name) _ : rest) -> Right (name, rest)
    _ -> Left (errorAt toks "expected identifier")

expectSymbol :: String -> [Token] -> Either CompileError [Token]
expectSymbol sym toks =
  case toks of
    (Token (TkSymbol s) _ : rest) | s == sym -> Right rest
    _ -> Left (errorAt toks ("expected symbol '" <> sym <> "'"))

parseScalar :: String -> Scalar
parseScalar name = case name of
  "i32" -> I32
  "u32" -> U32
  "f32" -> F32
  "bool" -> Bool
  _ -> I32

toBindingKind :: String -> Maybe String -> Either CompileError BindingKind
toBindingKind addrSpace access =
  case addrSpace of
    "uniform" -> Right BUniform
    "storage" -> case access of
      Just "read" -> Right BStorageRead
      Just "read_write" -> Right BStorageReadWrite
      Nothing -> Right BStorageRead
      Just other -> Left (CompileError ("unsupported storage access: " <> other) Nothing Nothing)
    "sampler" -> Right BSampler
    "texture" -> Right BTexture2D
    other -> Left (CompileError ("unsupported address space: " <> other) Nothing Nothing)

bindingKindFromType :: Type -> Either CompileError BindingKind
bindingKindFromType ty =
  case ty of
    TySampler -> Right BSampler
    TyTexture2D _ -> Right BTexture2D
    _ -> Left (CompileError "bindings without address space must be sampler or texture types" Nothing Nothing)

vecSize :: String -> Int
vecSize name = case name of
  "vec2" -> 2
  "vec3" -> 3
  "vec4" -> 4
  _ -> 4

bindingNumbers :: [Attr] -> Either CompileError (Word32, Word32)
bindingNumbers attrs =
  let grp = attrInt "group" attrs
      bind = attrInt "binding" attrs
  in case (grp, bind) of
       (Just g, Just b) -> Right (fromIntegral g, fromIntegral b)
       _ -> Left (CompileError "@group and @binding are required for bindings" Nothing Nothing)

entryAttributes :: [Attr] -> Either CompileError (Stage, Maybe (Word32, Word32, Word32))
entryAttributes attrs =
  let isCompute = any (\(Attr name _) -> name == "compute") attrs
      isFragment = any (\(Attr name _) -> name == "fragment") attrs
      isVertex = any (\(Attr name _) -> name == "vertex") attrs
      wg = attrInts "workgroup_size" attrs
  in case (isCompute, isFragment, isVertex) of
       (True, True, _) -> Left (CompileError "entry point cannot be both @compute and @fragment" Nothing Nothing)
       (True, _, True) -> Left (CompileError "entry point cannot be both @compute and @vertex" Nothing Nothing)
       (_, True, True) -> Left (CompileError "entry point cannot be both @fragment and @vertex" Nothing Nothing)
       (True, False, False) ->
         case wg of
           Just [x] -> Right (StageCompute, Just (fromIntegral x, 1, 1))
           Just [x, y] -> Right (StageCompute, Just (fromIntegral x, fromIntegral y, 1))
           Just [x, y, z] -> Right (StageCompute, Just (fromIntegral x, fromIntegral y, fromIntegral z))
           _ -> Left (CompileError "@workgroup_size is required for @compute" Nothing Nothing)
       (False, True, False) ->
         case wg of
           Nothing -> Right (StageFragment, Nothing)
           Just _ -> Left (CompileError "@workgroup_size is not allowed for @fragment" Nothing Nothing)
       (False, False, True) ->
         case wg of
           Nothing -> Right (StageVertex, Nothing)
           Just _ -> Left (CompileError "@workgroup_size is not allowed for @vertex" Nothing Nothing)
       (False, False, False) -> Left (CompileError "entry point must be @compute, @fragment, or @vertex" Nothing Nothing)

paramBuiltin :: [Attr] -> Maybe String
paramBuiltin = attrBuiltin

hasBuiltinGlobalId :: [Attr] -> Bool
hasBuiltinGlobalId attrs =
  paramBuiltin attrs == Just "global_invocation_id"

isVec3U32 :: Type -> Bool
isVec3U32 ty = case ty of
  TyVector 3 U32 -> True
  _ -> False

isU32 :: Type -> Bool
isU32 ty = case ty of
  TyScalar U32 -> True
  _ -> False

isVec4F32 :: Type -> Bool
isVec4F32 ty = case ty of
  TyVector 4 F32 -> True
  _ -> False

errorAt :: [Token] -> String -> CompileError
errorAt toks msg =
  case toks of
    (Token _ (SrcPos l c) : _) -> CompileError msg (Just l) (Just c)
    [] -> CompileError msg Nothing Nothing

renderError :: CompileError -> String
renderError err =
  let loc = case (ceLine err, ceColumn err) of
        (Just l, Just c) -> " at " <> show l <> ":" <> show c
        _ -> ""
  in ceMessage err <> loc

-- Attributes

data Attr = Attr String [AttrArg] deriving (Eq, Show)

data AttrArg = AttrInt Integer | AttrIdent String deriving (Eq, Show)

attrInt :: String -> [Attr] -> Maybe Integer
attrInt name attrs =
  case [v | Attr n args <- attrs, n == name, AttrInt v <- args] of
    (x:_) -> Just x
    _ -> Nothing

attrIntMaybe :: String -> [Attr] -> Maybe Word32
attrIntMaybe name attrs = fmap fromIntegral (attrInt name attrs)

attrBuiltin :: [Attr] -> Maybe String
attrBuiltin attrs =
  case [name | Attr n args <- attrs, n == "builtin", AttrIdent name <- args] of
    (x:_) -> Just x
    _ -> Nothing

attrInts :: String -> [Attr] -> Maybe [Integer]
attrInts name attrs =
  case [nums | Attr n args <- attrs, n == name, let nums = [v | AttrInt v <- args], not (null nums)] of
    (x:_) -> Just x
    _ -> Nothing

-- Interface and layout

buildInterface :: ModuleAst -> Either CompileError ShaderInterface
buildInterface modAst = do
  let structEnv = [(sdName s, s) | s <- modStructs modAst]
  bindings <- mapM (layoutBinding structEnv) (modBindings modAst)
  pure (ShaderInterface bindings)

layoutBinding :: [(String, StructDecl)] -> BindingDecl -> Either CompileError BindingInfo
layoutBinding env decl = do
  case bdKind decl of
    BUniform -> ensureStructBinding
    BStorageRead -> ensureStructBinding
    BStorageReadWrite -> ensureStructBinding
    BSampler ->
      case bdType decl of
        TySampler -> pure ()
        _ -> Left (CompileError "sampler bindings must use type sampler" Nothing Nothing)
    BTexture2D ->
      case bdType decl of
        TyTexture2D _ -> pure ()
        _ -> Left (CompileError "texture bindings must use type texture_2d<scalar>" Nothing Nothing)
  tyLayout <- resolveTypeLayout env [] (bdType decl)
  pure (BindingInfo (bdName decl) (bdKind decl) (bdGroup decl) (bdBinding decl) tyLayout)
  where
    ensureStructBinding =
      case bdType decl of
        TyStructRef _ -> pure ()
        _ -> Left (CompileError "bindings must use a struct type (wrap arrays in a struct)" Nothing Nothing)

resolveTypeLayout :: [(String, StructDecl)] -> [String] -> Type -> Either CompileError TypeLayout
resolveTypeLayout env stack ty =
  case ty of
    TyScalar s -> Right (TLScalar s 4 4)
    TyVector n s ->
      let (a, sz) = vectorLayout n
      in Right (TLVector n s a sz)
    TyArray elemTy mlen -> do
      elemLayout <- resolveTypeLayout env stack elemTy
      let elemAlign = layoutAlign elemLayout
      let elemSize = layoutSize elemLayout
      let stride = roundUp elemSize elemAlign
      let total = case mlen of
            Nothing -> stride
            Just n -> stride * fromIntegral n
      Right (TLArray mlen stride elemLayout elemAlign total)
    TySampler -> Right TLSampler
    TyTexture2D s -> Right (TLTexture2D s)
    TyStructRef name ->
      if name `elem` stack
        then Left (CompileError ("recursive struct: " <> name) Nothing Nothing)
        else case lookup name env of
          Nothing -> Left (CompileError ("unknown struct: " <> name) Nothing Nothing)
          Just decl -> do
            let stack' = name : stack
            fields <- resolveFields env stack' (sdFields decl)
            let align = maximum (1 : map (layoutAlign . flType) fields)
            let size = structSize fields align
            Right (TLStruct name fields align size)

resolveFields :: [(String, StructDecl)] -> [String] -> [FieldDecl] -> Either CompileError [FieldLayout]
resolveFields env stack fields =
  let go _ acc [] = Right (reverse acc)
      go offset acc (FieldDecl name fty : rest) = do
        fLayout <- resolveTypeLayout env stack fty
        if containsResource fLayout
          then Left (CompileError "resource types are not allowed in struct fields" Nothing Nothing)
          else do
            let aligned = roundUp offset (layoutAlign fLayout)
            let entry = FieldLayout name aligned fLayout
            let offset' = aligned + layoutSize fLayout
            go offset' (entry:acc) rest
  in go 0 [] fields

containsResource :: TypeLayout -> Bool
containsResource layout =
  case layout of
    TLSampler -> True
    TLTexture2D _ -> True
    TLArray _ _ elemLayout _ _ -> containsResource elemLayout
    TLStruct _ fields _ _ -> any (containsResource . flType) fields
    _ -> False

structSize :: [FieldLayout] -> Word32 -> Word32
structSize fields align =
  case fields of
    [] -> 0
    _ ->
      let lastField = last fields
          end = flOffset lastField + layoutSize (flType lastField)
      in roundUp end align

roundUp :: Word32 -> Word32 -> Word32
roundUp val align =
  if align == 0 then val else ((val + align - 1) `div` align) * align

-- SPIR-V emission

emitSpirv :: CompileOptions -> ModuleAst -> ShaderInterface -> Either CompileError ByteString
emitSpirv opts modAst iface = do
  entry <- case modEntry modAst of
    Nothing -> Left (CompileError "missing entry point" Nothing Nothing)
    Just e -> Right e
  let structEnv = [(sdName s, s) | s <- modStructs modAst]
  structLayouts <- mapM (resolveStructLayout structEnv) (modStructs modAst)
  retLayout <- case (epStage entry, epReturnType entry) of
    (StageFragment, Just ty) -> Just <$> resolveTypeLayout structEnv [] ty
    (StageFragment, Nothing) -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)
    (StageVertex, Just ty) -> Just <$> resolveTypeLayout structEnv [] ty
    (StageVertex, Nothing) -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)
    _ -> Right Nothing
  let blockStructs = [name | BindingInfo _ _ _ _ (TLStruct name _ _ _) <- siBindings iface]
  let state0 = emptyGenState structLayouts blockStructs
  let ((), state1) = emitStructs state0
  (envGlobals, _ifaceIds, outVar, state2) <- emitGlobals structEnv iface entry retLayout state1
  state3 <- emitMainFunction entry envGlobals outVar state2
  let spirvWords = buildSpirvWords opts entry state3
  pure (spirvToBytes spirvWords)

resolveStructLayout :: [(String, StructDecl)] -> StructDecl -> Either CompileError (String, TypeLayout)
resolveStructLayout env decl = do
  layout <- resolveTypeLayout env [] (TyStructRef (sdName decl))
  Right (sdName decl, layout)

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

opConstant :: Word16
opConstant = 43

opLoad :: Word16
opLoad = 61

opStore :: Word16
opStore = 62

opAccessChain :: Word16
opAccessChain = 65

opVariable :: Word16
opVariable = 59

opFunction :: Word16
opFunction = 54

opLabel :: Word16
opLabel = 248

opReturn :: Word16
opReturn = 253

opFunctionEnd :: Word16
opFunctionEnd = 56

opCompositeConstruct :: Word16
opCompositeConstruct = 80

opSampledImage :: Word16
opSampledImage = 86

opImageSampleImplicitLod :: Word16
opImageSampleImplicitLod = 87

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

capabilityShader :: Word32
capabilityShader = 1

addressingLogical :: Word32
addressingLogical = 0

memoryModelGLSL450 :: Word32
memoryModelGLSL450 = 1

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

decorationBlock :: Word32
decorationBlock = 2

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

decorationLocation :: Word32
decorationLocation = 30

builtInGlobalInvocationId :: Word32
builtInGlobalInvocationId = 28

builtInPosition :: Word32
builtInPosition = 0

builtInVertexIndex :: Word32
builtInVertexIndex = 42

dim2D :: Word32
dim2D = 1

imageFormatUnknown :: Word32
imageFormatUnknown = 0

functionControlNone :: Word32
functionControlNone = 0

-- Generator state

data GenState = GenState
  { gsNextId :: Word32
  , gsStructLayouts :: [(String, TypeLayout)]
  , gsStructIds :: [(String, Word32)]
  , gsBlockStructs :: [String]
  , gsTypeCache :: [(TypeKey, Word32)]
  , gsConstCache :: [(ConstKey, Word32)]
  , gsNames :: [Instr]
  , gsDecorations :: [Instr]
  , gsTypes :: [Instr]
  , gsConstants :: [Instr]
  , gsGlobals :: [Instr]
  , gsFunctions :: [Instr]
  , gsEntryPoint :: Maybe Word32
  , gsInterfaceIds :: [Word32]
  }

emptyGenState :: [(String, TypeLayout)] -> [String] -> GenState
emptyGenState structLayouts blockStructs =
  let (ids, nextId) = assignStructIds 1 structLayouts
  in GenState
      { gsNextId = nextId
      , gsStructLayouts = structLayouts
      , gsStructIds = ids
      , gsBlockStructs = blockStructs
      , gsTypeCache = []
      , gsConstCache = []
      , gsNames = []
      , gsDecorations = []
      , gsTypes = []
      , gsConstants = []
      , gsGlobals = []
      , gsFunctions = []
      , gsEntryPoint = Nothing
      , gsInterfaceIds = []
      }

assignStructIds :: Word32 -> [(String, TypeLayout)] -> ([(String, Word32)], Word32)
assignStructIds start layouts =
  let go next acc [] = (reverse acc, next)
      go next acc ((name, _):rest) = go (next + 1) ((name, next):acc) rest
  in go start [] layouts

freshId :: GenState -> (Word32, GenState)
freshId st = (gsNextId st, st { gsNextId = gsNextId st + 1 })

addInstr :: (GenState -> [Instr]) -> (GenState -> [Instr] -> GenState) -> Instr -> GenState -> GenState
addInstr getter setter instr st =
  let xs = getter st
  in setter st (xs <> [instr])

addName :: Instr -> GenState -> GenState
addName = addInstr gsNames (\st v -> st { gsNames = v })

addDecoration :: Instr -> GenState -> GenState
addDecoration = addInstr gsDecorations (\st v -> st { gsDecorations = v })

addType :: Instr -> GenState -> GenState
addType = addInstr gsTypes (\st v -> st { gsTypes = v })

addConst :: Instr -> GenState -> GenState
addConst = addInstr gsConstants (\st v -> st { gsConstants = v })

addGlobal :: Instr -> GenState -> GenState
addGlobal = addInstr gsGlobals (\st v -> st { gsGlobals = v })

-- Emit struct types with member decorations
emitStructs :: GenState -> ((), GenState)
emitStructs st0 =
  let st1 = foldl' emitStruct st0 (gsStructLayouts st0)
  in ((), st1)
  where
    emitStruct st (name, layout) =
      case layout of
        TLStruct _ fields _ _ ->
          let structId = fromMaybe (error "missing struct id") (lookup name (gsStructIds st))
              (st1, fieldTypeIds) = mapAccumL emitFieldType st fields
              st2 = addType (Instr opTypeStruct (structId : fieldTypeIds)) st1
              st3 = addName (Instr opName (structId : encodeString name)) st2
              st4 = foldl' (emitMemberDecorate structId) st3 (zip [0 :: Int ..] fields)
              st5 = if name `elem` gsBlockStructs st4
                then addDecoration (Instr opDecorate [structId, decorationBlock]) st4
                else st4
              st6 = foldl' (emitMemberName structId) st5 (zip [0 :: Int ..] fields)
          in st6
        _ -> st

    emitFieldType st field =
      let (tyId, st') = emitTypeFromLayout st (flType field)
      in (st', tyId)

    emitMemberDecorate structId st (ix, field) =
      let offset = flOffset field
      in addDecoration (Instr opMemberDecorate [structId, fromIntegral ix, decorationOffset, offset]) st

    emitMemberName structId st (ix, field) =
      addName (Instr opMemberName (structId : fromIntegral ix : encodeString (flName field))) st

data VarAccess = ReadOnly | ReadWrite
  deriving (Eq, Show)

data VarInfo = VarInfo
  { viType :: TypeLayout
  , viPtrId :: Word32
  , viStorage :: Word32
  , viAccess :: VarAccess
  } deriving (Eq, Show)

data Value = Value
  { valType :: TypeLayout
  , valId :: Word32
  } deriving (Eq, Show)

data FuncState = FuncState
  { fsLocals :: [Instr]
  , fsInstrs :: [Instr]
  , fsVars :: [(String, VarInfo)]
  , fsTerminated :: Bool
  } deriving (Eq, Show)

emitGlobals :: [(String, StructDecl)] -> ShaderInterface -> EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError ([(String, VarInfo)], [Word32], Maybe VarInfo, GenState)
emitGlobals structEnv iface entry retLayout st0 = do
  let (envBindings, idsBindings, st1) = foldl' emitBinding ([], [], st0) (siBindings iface)
  (envInputs, idsInputs, st2) <- emitEntryInputs structEnv entry st1
  (outVar, idsOut, st3) <- emitStageOutput entry retLayout st2
  let envAll = envBindings <> envInputs <> maybe [] (\v -> [("frag_output", v)]) outVar
  let ifaceIds = idsBindings <> idsInputs <> idsOut
  let st4 = st3 { gsInterfaceIds = ifaceIds }
  pure (envAll, ifaceIds, outVar, st4)
  where
    emitBinding (envAcc, idAcc, st) binding =
      let (ptrTy, st1) = emitPointerForBinding st (biKind binding) (biType binding)
          (varId, st2) = freshId st1
          storageClass = case biKind binding of
            BUniform -> storageClassUniform
            BStorageRead -> storageClassStorageBuffer
            BStorageReadWrite -> storageClassStorageBuffer
            BSampler -> storageClassUniformConstant
            BTexture2D -> storageClassUniformConstant
          st3 = addGlobal (Instr opVariable [ptrTy, varId, storageClass]) st2
          st4 = addDecoration (Instr opDecorate [varId, decorationDescriptorSet, biGroup binding]) st3
          st5 = addDecoration (Instr opDecorate [varId, decorationBinding, biBinding binding]) st4
          st6 = addName (Instr opName (varId : encodeString (biName binding))) st5
          access = case biKind binding of
            BStorageReadWrite -> ReadWrite
            _ -> ReadOnly
          info = VarInfo (biType binding) varId storageClass access
      in (envAcc <> [(biName binding, info)], idAcc <> [varId], st6)

emitEntryInputs :: [(String, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(String, VarInfo)], [Word32], GenState)
emitEntryInputs structEnv entry st0 =
  case epStage entry of
    StageCompute -> emitComputeInputs structEnv entry st0
    StageFragment -> emitFragmentInputs structEnv entry st0
    StageVertex -> emitVertexInputs structEnv entry st0

emitComputeInputs :: [(String, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(String, VarInfo)], [Word32], GenState)
emitComputeInputs _ entry st0 =
  case epParams entry of
    [Param name ty attrs]
      | hasBuiltinGlobalId attrs && isVec3U32 ty ->
          let (vecTy, st1) = emitVec3U32Type st0
              (ptrTy, st2) = emitPointerType st1 storageClassInput vecTy
              (varId, st3) = freshId st2
              st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassInput]) st3
              st5 = addDecoration (Instr opDecorate [varId, decorationBuiltIn, builtInGlobalInvocationId]) st4
              st6 = addName (Instr opName (varId : encodeString name)) st5
              info = VarInfo (TLVector 3 U32 16 16) varId storageClassInput ReadOnly
          in Right ([(name, info)], [varId], st6)
      | otherwise -> Left (CompileError "invalid compute entry parameter" Nothing Nothing)
    [] -> Right ([], [], st0)
    _ -> Left (CompileError "multiple parameters are not supported for @compute" Nothing Nothing)

emitFragmentInputs :: [(String, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(String, VarInfo)], [Word32], GenState)
emitFragmentInputs structEnv entry st0 = do
  let params = epParams entry
  (env, ids, st1) <- foldl' (emitParam structEnv) (Right ([], [], st0)) params
  pure (env, ids, st1)
  where
    emitParam envStruct acc param = do
      (envAcc, idAcc, st) <- acc
      loc <- case paramLocation (paramAttrs param) of
        Nothing -> Left (CompileError "fragment parameters must use @location" Nothing Nothing)
        Just n -> Right n
      layout <- resolveTypeLayout envStruct [] (paramType param)
      if containsResource layout
        then Left (CompileError "resource types are not allowed as fragment inputs" Nothing Nothing)
        else pure ()
      let (baseTy, st1) = emitTypeFromLayout st layout
      let (ptrTy, st2) = emitPointerType st1 storageClassInput baseTy
      let (varId, st3) = freshId st2
      let st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassInput]) st3
      let st5 = addDecoration (Instr opDecorate [varId, decorationLocation, loc]) st4
      let st6 = addName (Instr opName (varId : encodeString (paramName param))) st5
      let info = VarInfo layout varId storageClassInput ReadOnly
      Right (envAcc <> [(paramName param, info)], idAcc <> [varId], st6)

emitVertexInputs :: [(String, StructDecl)] -> EntryPoint -> GenState -> Either CompileError ([(String, VarInfo)], [Word32], GenState)
emitVertexInputs _ entry st0 =
  case epParams entry of
    [Param name ty attrs]
      | paramBuiltin attrs == Just "vertex_index" && isU32 ty ->
          let (u32Ty, st1) = emitIntType False st0
              (ptrTy, st2) = emitPointerType st1 storageClassInput u32Ty
              (varId, st3) = freshId st2
              st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassInput]) st3
              st5 = addDecoration (Instr opDecorate [varId, decorationBuiltIn, builtInVertexIndex]) st4
              st6 = addName (Instr opName (varId : encodeString name)) st5
              info = VarInfo (TLScalar U32 4 4) varId storageClassInput ReadOnly
          in Right ([(name, info)], [varId], st6)
      | otherwise -> Left (CompileError "invalid vertex entry parameter" Nothing Nothing)
    [] -> Right ([], [], st0)
    _ -> Left (CompileError "multiple parameters are not supported for @vertex" Nothing Nothing)

emitStageOutput :: EntryPoint -> Maybe TypeLayout -> GenState -> Either CompileError (Maybe VarInfo, [Word32], GenState)
emitStageOutput entry retLayout st0 =
  case epStage entry of
    StageCompute -> Right (Nothing, [], st0)
    StageFragment ->
      case retLayout of
        Nothing -> Left (CompileError "fragment entry point missing return type" Nothing Nothing)
        Just layout ->
          let (baseTy, st1) = emitTypeFromLayout st0 layout
              (ptrTy, st2) = emitPointerType st1 storageClassOutput baseTy
              (varId, st3) = freshId st2
              st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassOutput]) st3
              loc = fromMaybe 0 (epReturnLocation entry)
              st5 = addDecoration (Instr opDecorate [varId, decorationLocation, loc]) st4
              st6 = addName (Instr opName (varId : encodeString "frag_output")) st5
              info = VarInfo layout varId storageClassOutput ReadWrite
          in Right (Just info, [varId], st6)
    StageVertex ->
      case retLayout of
        Nothing -> Left (CompileError "vertex entry point missing return type" Nothing Nothing)
        Just layout ->
          case epReturnBuiltin entry of
            Just "position" ->
              let (baseTy, st1) = emitTypeFromLayout st0 layout
                  (ptrTy, st2) = emitPointerType st1 storageClassOutput baseTy
                  (varId, st3) = freshId st2
                  st4 = addGlobal (Instr opVariable [ptrTy, varId, storageClassOutput]) st3
                  st5 = addDecoration (Instr opDecorate [varId, decorationBuiltIn, builtInPosition]) st4
                  st6 = addName (Instr opName (varId : encodeString "position")) st5
                  info = VarInfo layout varId storageClassOutput ReadWrite
              in Right (Just info, [varId], st6)
            _ -> Left (CompileError "vertex entry point must return @builtin(position)" Nothing Nothing)

emitMainFunction :: EntryPoint -> [(String, VarInfo)] -> Maybe VarInfo -> GenState -> Either CompileError GenState
emitMainFunction entry env outVar st0 = do
  let (voidTy, st1) = emitVoidType st0
  let (fnTy, st2) = emitFunctionType st1 voidTy []
  let (fnId, st3) = freshId st2
  let (labelId, st4) = freshId st3
  let st5 = addName (Instr opName (fnId : encodeString (epName entry))) st4
  let fs0 = FuncState [] [] env False
  (st6, fs1) <- emitStatements entry outVar st5 fs0
  let funcInstrs =
        [ Instr opFunction [voidTy, fnId, functionControlNone, fnTy]
        , Instr opLabel [labelId]
        ] <> fsLocals fs1 <> fsInstrs fs1 <> [Instr opReturn [], Instr opFunctionEnd []]
  let st7 = addFunctions funcInstrs st6
  let st8 = st7 { gsEntryPoint = Just fnId }
  pure st8

addFunctions :: [Instr] -> GenState -> GenState
addFunctions instrs st = st { gsFunctions = gsFunctions st <> instrs }

addFuncInstr :: Instr -> FuncState -> FuncState
addFuncInstr instr fs = fs { fsInstrs = fsInstrs fs <> [instr] }

addFuncLocal :: Instr -> FuncState -> FuncState
addFuncLocal instr fs = fs { fsLocals = fsLocals fs <> [instr] }

emitStatements :: EntryPoint -> Maybe VarInfo -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitStatements entry outVar st fs = go st fs (epBody entry)
  where
    go st' fs' [] =
      case epStage entry of
        StageFragment ->
          if fsTerminated fs'
            then Right (st', fs')
            else Left (CompileError "fragment entry point must return a value" Nothing Nothing)
        StageVertex ->
          if fsTerminated fs'
            then Right (st', fs')
            else Left (CompileError "vertex entry point must return a value" Nothing Nothing)
        StageCompute -> Right (st', fs')
    go st' fs' (s:ss) = do
      (st1, fs1) <- emitStmt entry outVar st' fs' s
      go st1 fs1 ss

emitStmt :: EntryPoint -> Maybe VarInfo -> GenState -> FuncState -> Stmt -> Either CompileError (GenState, FuncState)
emitStmt entry outVar st fs stmt
  | fsTerminated fs = Left (CompileError "unreachable code after return" Nothing Nothing)
  | otherwise =
      case stmt of
        SLet name expr -> emitLocal name expr st fs
        SVar name expr -> emitLocal name expr st fs
        SAssign lv expr -> do
          (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
          (st2, fs2, val) <- emitExpr st1 fs1 expr
          ensureWritable ptrInfo
          ensureTypeMatch (viType ptrInfo) (valType val)
          let fs3 = addFuncInstr (Instr opStore [viPtrId ptrInfo, valId val]) fs2
          Right (st2, fs3)
        SReturn mexpr ->
          case epStage entry of
            StageCompute ->
              case mexpr of
                Nothing -> Right (st, fs { fsTerminated = True })
                Just _ -> Left (CompileError "compute entry points cannot return a value" Nothing Nothing)
            StageFragment -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "fragment entry points must return a value" Nothing Nothing)
                Just e -> Right e
              out <- case outVar of
                Nothing -> Left (CompileError "missing fragment output variable" Nothing Nothing)
                Just v -> Right v
              (st1, fs1, val) <- emitExpr st fs expr
              ensureTypeMatch (viType out) (valType val)
              let fs2 = addFuncInstr (Instr opStore [viPtrId out, valId val]) fs1
              Right (st1, fs2 { fsTerminated = True })
            StageVertex -> do
              expr <- case mexpr of
                Nothing -> Left (CompileError "vertex entry points must return a value" Nothing Nothing)
                Just e -> Right e
              out <- case outVar of
                Nothing -> Left (CompileError "missing vertex output variable" Nothing Nothing)
                Just v -> Right v
              (st1, fs1, val) <- emitExpr st fs expr
              ensureTypeMatch (viType out) (valType val)
              let fs2 = addFuncInstr (Instr opStore [viPtrId out, valId val]) fs1
              Right (st1, fs2 { fsTerminated = True })

emitLocal :: String -> Expr -> GenState -> FuncState -> Either CompileError (GenState, FuncState)
emitLocal name expr st fs = do
  (st1, fs1, val) <- emitExpr st fs expr
  let (baseTy, st2) = emitTypeFromLayout st1 (valType val)
  let (ptrTy, st3) = emitPointerType st2 storageClassFunction baseTy
  let (varId, st4) = freshId st3
  let fs2 = addFuncLocal (Instr opVariable [ptrTy, varId, storageClassFunction]) fs1
  let fs3 = addFuncInstr (Instr opStore [varId, valId val]) fs2
  let info = VarInfo (valType val) varId storageClassFunction ReadWrite
  let fs4 = fs3 { fsVars = (name, info) : fsVars fs3 }
  Right (st4, fs4)

emitExpr :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState, Value)
emitExpr st fs expr =
  case expr of
    EInt n -> do
      let val = fromIntegral n :: Word32
      let (cid, st1) = emitConstU32 st val
      let layout = TLScalar U32 4 4
      Right (st1, fs, Value layout cid)
    EFloat f -> do
      let (cid, st1) = emitConstF32 st f
      let layout = TLScalar F32 4 4
      Right (st1, fs, Value layout cid)
    EVar _ -> emitLoadFromExpr st fs expr
    EField _ _ -> emitLoadFromExpr st fs expr
    EIndex _ _ -> emitLoadFromExpr st fs expr
    EUnary OpNeg inner -> do
      (st1, fs1, val) <- emitExpr st fs inner
      opcode <- case classifyNumeric (valType val) of
        Nothing -> Left (CompileError "unary minus only supports scalar or vector numeric types" Nothing Nothing)
        Just (_, scalar) -> case scalar of
          F32 -> Right opFNegate
          I32 -> Right opSNegate
          U32 -> Left (CompileError "unary minus is not supported for u32" Nothing Nothing)
          Bool -> Left (CompileError "unary minus is not supported for bool" Nothing Nothing)
      let (tyId, st2) = emitTypeFromLayout st1 (valType val)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs1
      Right (st3, fs2, Value (valType val) resId)
    EBinary op lhs rhs -> do
      (st1, fs1, lval) <- emitExpr st fs lhs
      (st2, fs2, rval) <- emitExpr st1 fs1 rhs
      ensureTypeMatch (valType lval) (valType rval)
      emitBinary op (valType lval) (valId lval) (valId rval) st2 fs2
    ECall name args -> emitCall name args st fs

emitLoadFromExpr :: GenState -> FuncState -> Expr -> Either CompileError (GenState, FuncState, Value)
emitLoadFromExpr st fs expr =
  case exprToLValue expr of
    Nothing -> Left (CompileError "expected addressable expression" Nothing Nothing)
    Just lv -> do
      (st1, fs1, ptrInfo) <- emitLValuePtr st fs lv
      let (tyId, st2) = emitTypeFromLayout st1 (viType ptrInfo)
      let (resId, st3) = freshId st2
      let fs2 = addFuncInstr (Instr opLoad [tyId, resId, viPtrId ptrInfo]) fs1
      Right (st3, fs2, Value (viType ptrInfo) resId)

emitBinary :: BinOp -> TypeLayout -> Word32 -> Word32 -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitBinary op layout lhs rhs st fs =
  case classifyNumeric layout of
    Nothing -> Left (CompileError "binary operators only support scalar or vector numeric types" Nothing Nothing)
    Just (n, scalar) -> do
      let (tyId, st1) = emitTypeFromLayout st layout
      let (resId, st2) = freshId st1
      let opcode = case (scalar, op) of
            (F32, OpAdd) -> opFAdd
            (F32, OpSub) -> opFSub
            (F32, OpMul) -> opFMul
            (F32, OpDiv) -> opFDiv
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
      let outLayout = case n of
            1 -> layout
            _ -> layout
      Right (st2, fs1, Value outLayout resId)

emitCall :: String -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitCall name args st fs =
  case name of
    "vec2" -> emitVectorCtor 2 args st fs
    "vec3" -> emitVectorCtor 3 args st fs
    "vec4" -> emitVectorCtor 4 args st fs
    "f32" -> emitScalarCtor F32 args st fs
    "u32" -> emitScalarCtor U32 args st fs
    "i32" -> emitScalarCtor I32 args st fs
    "textureSample" -> emitTextureSample args st fs
    _ -> Left (CompileError ("unsupported call: " <> name) Nothing Nothing)

emitVectorCtor :: Int -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitVectorCtor n args st fs =
  if length args /= n
    then Left (CompileError "vector constructor arity mismatch" Nothing Nothing)
    else do
      (st1, fs1, vals) <- emitExprList st fs args
      case vals of
        [] -> Left (CompileError "vector constructor needs arguments" Nothing Nothing)
        (v:vs) -> do
          mapM_ (\v' -> ensureTypeMatch (valType v) (valType v')) vs
          scalar <- case valType v of
            TLScalar s _ _ -> Right s
            _ -> Left (CompileError "vector constructor arguments must be scalars" Nothing Nothing)
          let (align, size) = vectorLayout n
          let layout = TLVector n scalar align size
          let (tyId, st2) = emitTypeFromLayout st1 layout
          let (resId, st3) = freshId st2
          let fs2 = addFuncInstr (Instr opCompositeConstruct (tyId : resId : map valId vals)) fs1
          Right (st3, fs2, Value layout resId)

emitScalarCtor :: Scalar -> [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitScalarCtor scalar args st fs =
  case args of
    [arg] -> do
      (st1, fs1, val) <- emitExpr st fs arg
      case valType val of
        TLScalar s _ _ | s == scalar -> Right (st1, fs1, val)
        TLScalar s _ _ -> emitScalarConvert s scalar val st1 fs1
        _ -> Left (CompileError "scalar cast requires a scalar argument" Nothing Nothing)
    _ -> Left (CompileError "scalar cast requires a single argument" Nothing Nothing)

emitScalarConvert :: Scalar -> Scalar -> Value -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitScalarConvert from to val st fs = do
  opcode <- case (from, to) of
    (U32, F32) -> Right opConvertUToF
    (I32, F32) -> Right opConvertSToF
    (F32, U32) -> Right opConvertFToU
    (F32, I32) -> Right opConvertFToS
    _ -> Left (CompileError "unsupported scalar conversion" Nothing Nothing)
  let layout = TLScalar to 4 4
  let (tyId, st1) = emitTypeFromLayout st layout
  let (resId, st2) = freshId st1
  let fs1 = addFuncInstr (Instr opcode [tyId, resId, valId val]) fs
  Right (st2, fs1, Value layout resId)

emitTextureSample :: [Expr] -> GenState -> FuncState -> Either CompileError (GenState, FuncState, Value)
emitTextureSample args st fs =
  case args of
    [texExpr, samplerExpr, coordExpr] -> do
      (st1, fs1, texVal) <- emitExpr st fs texExpr
      (st2, fs2, sampVal) <- emitExpr st1 fs1 samplerExpr
      (st3, fs3, coordVal) <- emitExpr st2 fs2 coordExpr
      scalar <- case valType texVal of
        TLTexture2D s -> Right s
        _ -> Left (CompileError "textureSample expects a texture_2d binding" Nothing Nothing)
      case valType sampVal of
        TLSampler -> Right ()
        _ -> Left (CompileError "textureSample expects a sampler binding" Nothing Nothing)
      case valType coordVal of
        TLVector 2 F32 _ _ -> Right ()
        _ -> Left (CompileError "textureSample expects vec2<f32> coordinates" Nothing Nothing)
      if scalar /= F32
        then Left (CompileError "only texture_2d<f32> is supported for sampling" Nothing Nothing)
        else do
          let (sampledTy, st4) = emitSampledImageType scalar st3
          let (sampledId, st5) = freshId st4
          let fs4 = addFuncInstr (Instr opSampledImage [sampledTy, sampledId, valId texVal, valId sampVal]) fs3
          let (align, size) = vectorLayout 4
          let outLayout = TLVector 4 scalar align size
          let (outTy, st6) = emitTypeFromLayout st5 outLayout
          let (resId, st7) = freshId st6
          let fs5 = addFuncInstr (Instr opImageSampleImplicitLod [outTy, resId, sampledId, valId coordVal]) fs4
          Right (st7, fs5, Value outLayout resId)
    _ -> Left (CompileError "textureSample expects (texture, sampler, coords)" Nothing Nothing)

emitExprList :: GenState -> FuncState -> [Expr] -> Either CompileError (GenState, FuncState, [Value])
emitExprList st fs [] = Right (st, fs, [])
emitExprList st fs (x:xs) = do
  (st1, fs1, v) <- emitExpr st fs x
  (st2, fs2, vs) <- emitExprList st1 fs1 xs
  Right (st2, fs2, v:vs)

emitLValuePtr :: GenState -> FuncState -> LValue -> Either CompileError (GenState, FuncState, VarInfo)
emitLValuePtr st fs lv =
  case lv of
    LVVar name ->
      case lookup name (fsVars fs) of
        Just v -> Right (st, fs, v)
        Nothing -> Left (CompileError ("unknown variable: " <> name) Nothing Nothing)
    LVField base field -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      case viType baseInfo of
        TLStruct _ fields _ _ -> do
          (ix, fieldLayout) <- findField field fields
          let (ixId, st2) = emitConstU32 st1 (fromIntegral ix)
          emitAccessChain st2 fs1 baseInfo [ixId] fieldLayout
        TLVector n scalar _ _ -> do
          ix <- vectorFieldIndex field n
          let (ixId, st2) = emitConstU32 st1 (fromIntegral ix)
          let fieldLayout = TLScalar scalar 4 4
          emitAccessChain st2 fs1 baseInfo [ixId] fieldLayout
        _ -> Left (CompileError "field access requires struct or vector type" Nothing Nothing)
    LVIndex base idxExpr -> do
      (st1, fs1, baseInfo) <- emitLValuePtr st fs base
      (st2, fs2, idxVal) <- emitExpr st1 fs1 idxExpr
      ensureIndexType (valType idxVal)
      case viType baseInfo of
        TLArray _ _ elemLayout _ _ -> emitAccessChain st2 fs2 baseInfo [valId idxVal] elemLayout
        TLVector _ scalar _ _ ->
          let elemLayout = TLScalar scalar 4 4
          in emitAccessChain st2 fs2 baseInfo [valId idxVal] elemLayout
        _ -> Left (CompileError "indexing requires array or vector type" Nothing Nothing)

emitAccessChain :: GenState -> FuncState -> VarInfo -> [Word32] -> TypeLayout -> Either CompileError (GenState, FuncState, VarInfo)
emitAccessChain st fs baseInfo indices elemLayout = do
  let storageClass = viStorage baseInfo
  let (elemTy, st1) = emitTypeFromLayout st elemLayout
  let (ptrTy, st2) = emitPointerType st1 storageClass elemTy
  let (resId, st3) = freshId st2
  let instr = Instr opAccessChain (ptrTy : resId : viPtrId baseInfo : indices)
  let fs1 = addFuncInstr instr fs
  Right (st3, fs1, VarInfo elemLayout resId storageClass (viAccess baseInfo))

exprToLValue :: Expr -> Maybe LValue
exprToLValue expr =
  case expr of
    EVar name -> Just (LVVar name)
    EField base field -> LVField <$> exprToLValue base <*> pure field
    EIndex base idx -> LVIndex <$> exprToLValue base <*> pure idx
    _ -> Nothing

ensureTypeMatch :: TypeLayout -> TypeLayout -> Either CompileError ()
ensureTypeMatch a b =
  if a == b
    then Right ()
    else Left (CompileError "type mismatch" Nothing Nothing)

ensureWritable :: VarInfo -> Either CompileError ()
ensureWritable info =
  case viAccess info of
    ReadWrite -> Right ()
    ReadOnly -> Left (CompileError "assignment target is read-only" Nothing Nothing)

ensureIndexType :: TypeLayout -> Either CompileError ()
ensureIndexType layout =
  case layout of
    TLScalar U32 _ _ -> Right ()
    TLScalar I32 _ _ -> Right ()
    _ -> Left (CompileError "index must be u32 or i32" Nothing Nothing)

findField :: String -> [FieldLayout] -> Either CompileError (Int, TypeLayout)
findField name fields = go 0 fields
  where
    go _ [] = Left (CompileError ("unknown field: " <> name) Nothing Nothing)
    go ix (f:fs)
      | flName f == name = Right (ix, flType f)
      | otherwise = go (ix + 1) fs

vectorFieldIndex :: String -> Int -> Either CompileError Int
vectorFieldIndex field n =
  case field of
    "x" -> check 0
    "y" -> check 1
    "z" -> check 2
    "w" -> check 3
    _ -> Left (CompileError ("unknown vector field: " <> field) Nothing Nothing)
  where
    check ix =
      if ix < n
        then Right ix
        else Left (CompileError ("vector field out of range: " <> field) Nothing Nothing)

classifyNumeric :: TypeLayout -> Maybe (Int, Scalar)
classifyNumeric layout =
  case layout of
    TLScalar s _ _ -> Just (1, s)
    TLVector n s _ _ -> Just (n, s)
    _ -> Nothing

buildSpirvWords :: CompileOptions -> EntryPoint -> GenState -> [Word32]
buildSpirvWords opts entry st =
  let header =
        [ 0x07230203
        , spirvVersion opts
        , 0
        , gsNextId st
        , 0
        ]
      entryPointInstr = case gsEntryPoint st of
        Nothing -> []
        Just epId ->
          let nameWords = encodeString (epName entry)
              model = case epStage entry of
                StageCompute -> executionModelGLCompute
                StageFragment -> executionModelFragment
                StageVertex -> executionModelVertex
              operands = model : epId : nameWords <> gsInterfaceIds st
          in encodeInstr (Instr opEntryPoint operands)
      execModeInstr = case gsEntryPoint st of
        Nothing -> []
        Just epId ->
          case epStage entry of
            StageCompute ->
              case epWorkgroupSize entry of
                Nothing -> []
                Just (x, y, z) ->
                  let ops = [epId, executionModeLocalSize, x, y, z]
                  in encodeInstr (Instr opExecutionMode ops)
            StageFragment ->
              encodeInstr (Instr opExecutionMode [epId, executionModeOriginUpperLeft])
            StageVertex -> []
      body =
        concat
          [ encodeInstr (Instr opCapability [capabilityShader])
          , encodeInstr (Instr opMemoryModel [addressingLogical, memoryModelGLSL450])
          , entryPointInstr
          , execModeInstr
          , concatMap encodeInstr (gsNames st)
          , concatMap encodeInstr (gsDecorations st)
          , concatMap encodeInstr (gsTypes st)
          , concatMap encodeInstr (gsConstants st)
          , concatMap encodeInstr (gsGlobals st)
          , concatMap encodeInstr (gsFunctions st)
          ]
  in header <> body

emitVoidType :: GenState -> (Word32, GenState)
emitVoidType st = emitTypeCached st TKVoid (Instr opTypeVoid [])

emitBoolType :: GenState -> (Word32, GenState)
emitBoolType st = emitTypeCached st TKBool (Instr opTypeBool [])

emitIntType :: Bool -> GenState -> (Word32, GenState)
emitIntType signed st =
  let key = TKInt signed 32
      instr = Instr opTypeInt [32, if signed then 1 else 0]
  in emitTypeCached st key instr

emitFloatType :: GenState -> (Word32, GenState)
emitFloatType st =
  emitTypeCached st (TKFloat 32) (Instr opTypeFloat [32])

emitVecType :: Word32 -> Word32 -> GenState -> (Word32, GenState)
emitVecType elemId n st =
  emitTypeCached st (TKVector elemId n) (Instr opTypeVector [elemId, n])

emitSamplerType :: GenState -> (Word32, GenState)
emitSamplerType st =
  emitTypeCached st TKSampler (Instr opTypeSampler [])

emitImage2DType :: Scalar -> GenState -> (Word32, GenState)
emitImage2DType scalar st =
  let (sampledId, st1) = emitTypeFromLayout st (TLScalar scalar 4 4)
      instr = Instr opTypeImage [sampledId, dim2D, 0, 0, 0, 1, imageFormatUnknown]
  in emitTypeCached st1 (TKImage2D scalar) instr

emitSampledImageType :: Scalar -> GenState -> (Word32, GenState)
emitSampledImageType scalar st =
  let (imageId, st1) = emitImage2DType scalar st
      instr = Instr opTypeSampledImage [imageId]
  in emitTypeCached st1 (TKSampledImage scalar) instr

emitArrayType :: Word32 -> Word32 -> Word32 -> GenState -> (Word32, GenState)
emitArrayType elemId lenId stride st =
  let key = TKArray elemId (Just lenId) stride
      instr = Instr opTypeArray [elemId, lenId]
  in case lookup key (gsTypeCache st) of
       Just tid -> (tid, st)
       Nothing ->
         let (tyId, st1) = emitTypeCached st key instr
             st2 = addDecoration (Instr opDecorate [tyId, decorationArrayStride, stride]) st1
         in (tyId, st2)

emitRuntimeArrayType :: Word32 -> Word32 -> GenState -> (Word32, GenState)
emitRuntimeArrayType elemId stride st =
  let key = TKArray elemId Nothing stride
      instr = Instr opTypeRuntimeArray [elemId]
  in case lookup key (gsTypeCache st) of
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
    TLVector n s _ _ ->
      let (elemId, st1) = emitTypeFromLayout st (TLScalar s 4 4)
      in emitVecType elemId (fromIntegral n) st1
    TLSampler -> emitSamplerType st
    TLTexture2D s -> emitImage2DType s st
    TLArray mlen stride elemLayout _ _ ->
      let (elemId, st1) = emitTypeFromLayout st elemLayout
      in case mlen of
        Nothing -> emitRuntimeArrayType elemId stride st1
        Just n ->
          let (lenId, st2) = emitConstU32 st1 (fromIntegral n)
          in emitArrayType elemId lenId stride st2
    TLStruct name _ _ _ ->
      case lookup name (gsStructIds st) of
        Just sid -> (sid, st)
        Nothing -> error "struct id missing"

emitPointerForBinding :: GenState -> BindingKind -> TypeLayout -> (Word32, GenState)
emitPointerForBinding st kind layout =
  let storageClass = case kind of
        BUniform -> storageClassUniform
        BStorageRead -> storageClassStorageBuffer
        BStorageReadWrite -> storageClassStorageBuffer
        BSampler -> storageClassUniformConstant
        BTexture2D -> storageClassUniformConstant
      (baseId, st1) = emitTypeFromLayout st layout
  in emitPointerType st1 storageClass baseId

emitVec3U32Type :: GenState -> (Word32, GenState)
emitVec3U32Type st =
  let (u32Id, st1) = emitIntType False st
  in emitVecType u32Id 3 st1

emitConstU32 :: GenState -> Word32 -> (Word32, GenState)
emitConstU32 st val =
  emitConst st (ConstU32 val) $ \cid st1 ->
    let (u32Id, st2) = emitIntType False st1
        instr = Instr opConstant [u32Id, cid, val]
    in (instr, st2)

emitConstF32 :: GenState -> Float -> (Word32, GenState)
emitConstF32 st val =
  let bits = castFloatToWord32 val
  in emitConst st (ConstF32 bits) $ \cid st1 ->
       let (f32Id, st2) = emitFloatType st1
           instr = Instr opConstant [f32Id, cid, bits]
       in (instr, st2)

emitConst :: GenState -> ConstKey -> (Word32 -> GenState -> (Instr, GenState)) -> (Word32, GenState)
emitConst st key build =
  case lookup key (gsConstCache st) of
    Just cid -> (cid, st)
    Nothing ->
      let (id', st1) = freshId st
          (instr, st2) = build id' st1
          st3 = addConst instr st2
          st4 = st3 { gsConstCache = (key, id') : gsConstCache st3 }
      in (id', st4)

emitTypeCached :: GenState -> TypeKey -> Instr -> (Word32, GenState)
emitTypeCached st key instr =
  case lookup key (gsTypeCache st) of
    Just tid -> (tid, st)
    Nothing ->
      let (tid, st1) = freshId st
          st2 = addType (addResultId tid instr) st1
          st3 = st2 { gsTypeCache = (key, tid) : gsTypeCache st2 }
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
  | TKSampler
  | TKImage2D Scalar
  | TKSampledImage Scalar
  | TKArray Word32 (Maybe Word32) Word32
  | TKPointer Word32 Word32
  | TKFunction Word32 [Word32]
  deriving (Eq, Show)

data ConstKey
  = ConstU32 Word32
  | ConstI32 Word32
  | ConstF32 Word32
  deriving (Eq, Show)

-- Helpers

spirvToBytes :: [Word32] -> ByteString
spirvToBytes words32 = BS.pack (concatMap wordToBytes words32)

wordToBytes :: Word32 -> [Word8]
wordToBytes w =
  [ fromIntegral (w .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 24) .&. 0xFF)
  ]

encodeString :: String -> [Word32]
encodeString str =
  let bytes = map (fromIntegral . fromEnum) (str <> "\0")
      chunks = chunk4 bytes
  in map packWord chunks

chunk4 :: [Word32] -> [[Word32]]
chunk4 [] = []
chunk4 xs = take 4 (xs <> repeat 0) : chunk4 (drop 4 xs)

packWord :: [Word32] -> Word32
packWord [a, b, c, d] = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)
packWord _ = 0

bytesToExp :: ByteString -> Q Exp
bytesToExp bytes = do
  let ints = map (fromIntegral :: Word8 -> Integer) (BS.unpack bytes)
  listExp <- TH.listE (map (TH.litE . TH.integerL) ints)
  let listSig = TH.SigE listExp (TH.AppT TH.ListT (TH.ConT ''Word8))
  pure (TH.AppE (TH.VarE 'BS.pack) listSig)

interfaceToExp :: ShaderInterface -> Q Exp
interfaceToExp (ShaderInterface bindings) = do
  bindingsExp <- TH.listE (map bindingToExp bindings)
  pure (TH.AppE (TH.ConE 'ShaderInterface) bindingsExp)

bindingToExp :: BindingInfo -> Q Exp
bindingToExp info =
  TH.recConE 'BindingInfo
    [ TH.fieldExp 'biName (TH.litE (TH.stringL (biName info)))
    , TH.fieldExp 'biKind (TH.conE (bindingKindCon (biKind info)))
    , TH.fieldExp 'biGroup (TH.litE (TH.integerL (fromIntegral (biGroup info))))
    , TH.fieldExp 'biBinding (TH.litE (TH.integerL (fromIntegral (biBinding info))))
    , TH.fieldExp 'biType (typeLayoutToExp (biType info))
    ]

bindingKindCon :: BindingKind -> TH.Name
bindingKindCon kind = case kind of
  BUniform -> 'BUniform
  BStorageRead -> 'BStorageRead
  BStorageReadWrite -> 'BStorageReadWrite
  BSampler -> 'BSampler
  BTexture2D -> 'BTexture2D

typeLayoutToExp :: TypeLayout -> Q Exp
typeLayoutToExp layout =
  case layout of
    TLScalar s a sz ->
      TH.appE (TH.appE (TH.appE (TH.conE 'TLScalar) (scalarToExp s)) (TH.litE (TH.integerL (fromIntegral a))))
        (TH.litE (TH.integerL (fromIntegral sz)))
    TLVector n s a sz ->
      TH.appE
        (TH.appE (TH.appE (TH.appE (TH.conE 'TLVector) (TH.litE (TH.integerL (fromIntegral n)))) (scalarToExp s)) (TH.litE (TH.integerL (fromIntegral a))))
        (TH.litE (TH.integerL (fromIntegral sz)))
    TLArray mlen stride elemLayout a sz -> do
      elemExp <- typeLayoutToExp elemLayout
      mlenExp <- case mlen of
        Nothing -> TH.conE 'Nothing
        Just n -> TH.appE (TH.conE 'Just) (TH.litE (TH.integerL (fromIntegral n)))
      let exp1 = TH.AppE (TH.ConE 'TLArray) mlenExp
          exp2 = TH.AppE exp1 (TH.LitE (TH.integerL (fromIntegral stride)))
          exp3 = TH.AppE exp2 elemExp
          exp4 = TH.AppE exp3 (TH.LitE (TH.integerL (fromIntegral a)))
          exp5 = TH.AppE exp4 (TH.LitE (TH.integerL (fromIntegral sz)))
      pure exp5
    TLStruct name fields a sz -> do
      fieldsExp <- TH.listE (map fieldLayoutToExp fields)
      let exp1 = TH.AppE (TH.ConE 'TLStruct) (TH.LitE (TH.stringL name))
          exp2 = TH.AppE exp1 fieldsExp
          exp3 = TH.AppE exp2 (TH.LitE (TH.integerL (fromIntegral a)))
          exp4 = TH.AppE exp3 (TH.LitE (TH.integerL (fromIntegral sz)))
      pure exp4
    TLSampler -> TH.conE 'TLSampler
    TLTexture2D s -> TH.appE (TH.conE 'TLTexture2D) (scalarToExp s)

fieldLayoutToExp :: FieldLayout -> Q Exp
fieldLayoutToExp fld =
  TH.recConE 'FieldLayout
    [ TH.fieldExp 'flName (TH.litE (TH.stringL (flName fld)))
    , TH.fieldExp 'flOffset (TH.litE (TH.integerL (fromIntegral (flOffset fld))))
    , TH.fieldExp 'flType (typeLayoutToExp (flType fld))
    ]

scalarToExp :: Scalar -> Q Exp
scalarToExp s = case s of
  I32 -> TH.conE 'I32
  U32 -> TH.conE 'U32
  F32 -> TH.conE 'F32
  Bool -> TH.conE 'Bool

interfaceToType :: ShaderInterface -> TH.Type
interfaceToType (ShaderInterface bindings) =
  foldr
    (\b acc -> TH.AppT (TH.AppT TH.PromotedConsT (bindingToType b)) acc)
    TH.PromotedNilT
    bindings

bindingToType :: BindingInfo -> TH.Type
bindingToType info =
  foldl'
    TH.AppT
    (TH.PromotedT 'Binding)
    [ TH.LitT (TH.StrTyLit (biName info))
    , TH.PromotedT (bindingKindTypeCon (biKind info))
    , TH.LitT (TH.NumTyLit (fromIntegral (biGroup info)))
    , TH.LitT (TH.NumTyLit (fromIntegral (biBinding info)))
    , typeLayoutToType (biType info)
    ]

bindingKindTypeCon :: BindingKind -> TH.Name
bindingKindTypeCon kind = case kind of
  BUniform -> 'BUniform
  BStorageRead -> 'BStorageRead
  BStorageReadWrite -> 'BStorageReadWrite
  BSampler -> 'BSampler
  BTexture2D -> 'BTexture2D

typeLayoutToType :: TypeLayout -> TH.Type
typeLayoutToType layout =
  case layout of
    TLScalar s _ _ -> TH.AppT (TH.PromotedT 'TScalar) (scalarTypeToType s)
    TLVector n s _ _ ->
      TH.AppT
        (TH.AppT (TH.PromotedT 'TVec) (TH.LitT (TH.NumTyLit (fromIntegral n))))
        (scalarTypeToType s)
    TLArray mlen _ elemLayout _ _ ->
      case mlen of
        Nothing -> TH.AppT (TH.PromotedT 'TRuntimeArray) (typeLayoutToType elemLayout)
        Just n -> TH.AppT (TH.AppT (TH.PromotedT 'TArray) (TH.LitT (TH.NumTyLit (fromIntegral n)))) (typeLayoutToType elemLayout)
    TLStruct _ fields _ _ ->
      let fieldTypes = foldr (\f acc -> TH.AppT (TH.AppT TH.PromotedConsT (fieldLayoutToType f)) acc) TH.PromotedNilT fields
      in TH.AppT (TH.PromotedT 'TStruct) fieldTypes
    TLSampler -> TH.PromotedT 'TSampler
    TLTexture2D s -> TH.AppT (TH.PromotedT 'TTexture2D) (scalarTypeToType s)

fieldLayoutToType :: FieldLayout -> TH.Type
fieldLayoutToType fld =
  TH.AppT
    (TH.AppT (TH.PromotedT 'Field) (TH.LitT (TH.StrTyLit (flName fld))))
    (typeLayoutToType (flType fld))

scalarTypeToType :: Scalar -> TH.Type
scalarTypeToType s =
  TH.PromotedT $ case s of
    I32 -> 'SI32
    U32 -> 'SU32
    F32 -> 'SF32
    Bool -> 'SBool
