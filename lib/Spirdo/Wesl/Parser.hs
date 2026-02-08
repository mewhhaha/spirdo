{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parser for WESL source.
module Spirdo.Wesl.Parser where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit)
import Data.List (partition)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Word (Word32)
import Spirdo.Wesl.Syntax
import Spirdo.Wesl.Types
import Spirdo.Wesl.Util

parseModuleWith :: [String] -> String -> Either CompileError ModuleAst
parseModuleWith features src = do
  toks <- lexWesl (T.pack src)
  parseModuleTokensWith (Set.fromList (map T.pack features)) toks

type FeatureSet = Set.Set Text

posOf :: [Token] -> SrcPos
posOf toks =
  case toks of
    (Token _ pos : _) -> pos
    [] -> SrcPos 1 1

lexWesl :: Text -> Either CompileError [Token]
lexWesl = go (SrcPos 1 1)
  where
    go _ src
      | T.null src = pure []
    go pos src =
      case T.uncons src of
        Nothing -> pure []
        Just (c, cs)
          | c == ' ' || c == '\t' || c == '\n' || c == '\r' ->
              go (advance pos c) cs
          | isAlpha c || c == '_' ->
              let (ident, rest, pos') = consumeIdent pos src
              in (Token (TkIdent ident) pos :) <$> go pos' rest
          | isDigit c ->
              let (tok, rest, pos') = consumeNumber pos src
              in (Token tok pos :) <$> go pos' rest
          | c == '"' ->
              (\(str, rest, pos') -> (Token (TkString str) pos :) <$> go pos' rest)
                =<< consumeString pos cs
          | otherwise ->
              case T.uncons cs of
                Nothing ->
                  if isSymbolChar c
                    then emit1 pos c cs
                    else Left (CompileError ("unexpected character: " <> [c]) (Just pos.spLine) (Just pos.spCol))
                Just (c2, rest2) ->
                  case c of
                    '/' | c2 == '/' ->
                      let (_, rest, pos') = consumeLine (advance (advance pos '/') '/') rest2
                      in go pos' rest
                    '/' | c2 == '*' ->
                      (\(rest, pos') -> go pos' rest)
                        =<< consumeBlockComment (advance (advance pos '/') '*') rest2
                    ':' | c2 == ':' -> emit2 pos "::" c c2 rest2
                    '=' | c2 == '=' -> emit2 pos "==" c c2 rest2
                    '!' | c2 == '=' -> emit2 pos "!=" c c2 rest2
                    '<' | c2 == '=' -> emit2 pos "<=" c c2 rest2
                    '>' | c2 == '=' -> emit2 pos ">=" c c2 rest2
                    '&' | c2 == '&' -> emit2 pos "&&" c c2 rest2
                    '|' | c2 == '|' -> emit2 pos "||" c c2 rest2
                    '<' | c2 == '<' ->
                      case T.uncons rest2 of
                        Just (c3, rest3) | c3 == '=' -> emit3 pos "<<=" c c2 c3 rest3
                        _ -> emit2 pos "<<" c c2 rest2
                    '>' | c2 == '>' ->
                      case T.uncons rest2 of
                        Just (c3, rest3) | c3 == '=' -> emit3 pos ">>=" c c2 c3 rest3
                        _ -> emit2 pos ">>" c c2 rest2
                    '+' | c2 == '+' -> emit2 pos "++" c c2 rest2
                    '-' | c2 == '-' -> emit2 pos "--" c c2 rest2
                    '+' | c2 == '=' -> emit2 pos "+=" c c2 rest2
                    '-' | c2 == '=' -> emit2 pos "-=" c c2 rest2
                    '*' | c2 == '=' -> emit2 pos "*=" c c2 rest2
                    '/' | c2 == '=' -> emit2 pos "/=" c c2 rest2
                    '%' | c2 == '=' -> emit2 pos "%=" c c2 rest2
                    '&' | c2 == '=' -> emit2 pos "&=" c c2 rest2
                    '|' | c2 == '=' -> emit2 pos "|=" c c2 rest2
                    '^' | c2 == '=' -> emit2 pos "^=" c c2 rest2
                    _ ->
                      if isSymbolChar c
                        then emit1 pos c cs
                        else Left (CompileError ("unexpected character: " <> [c]) (Just pos.spLine) (Just pos.spCol))

    isSymbolChar ch =
      case ch of
        '@' -> True
        ':' -> True
        '{' -> True
        '}' -> True
        '(' -> True
        ')' -> True
        ';' -> True
        ',' -> True
        '<' -> True
        '>' -> True
        '=' -> True
        '[' -> True
        ']' -> True
        '+' -> True
        '-' -> True
        '*' -> True
        '/' -> True
        '.' -> True
        '%' -> True
        '!' -> True
        '&' -> True
        '|' -> True
        '^' -> True
        _ -> False

    symbolText ch =
      case ch of
        '@' -> "@"
        ':' -> ":"
        '{' -> "{"
        '}' -> "}"
        '(' -> "("
        ')' -> ")"
        ';' -> ";"
        ',' -> ","
        '<' -> "<"
        '>' -> ">"
        '=' -> "="
        '[' -> "["
        ']' -> "]"
        '+' -> "+"
        '-' -> "-"
        '*' -> "*"
        '/' -> "/"
        '.' -> "."
        '%' -> "%"
        '!' -> "!"
        '&' -> "&"
        '|' -> "|"
        '^' -> "^"
        _ -> T.singleton ch

    emit1 p ch rest =
      let sym = symbolText ch
      in (Token (TkSymbol sym) p :) <$> go (advance p ch) rest

    advance2 p a = advance (advance p a)
    advance3 p a b = advance (advance2 p a b)

    emit2 p sym c1 c2 rest =
      (Token (TkSymbol sym) p :) <$> go (advance2 p c1 c2) rest

    emit3 p sym c1 c2 c3 rest =
      (Token (TkSymbol sym) p :) <$> go (advance3 p c1 c2 c3) rest

    consumeLine p rest =
      let (line, rest') = T.break (== '\n') rest
          pos' = T.foldl' advance p line
      in if T.null rest'
           then (line, T.empty, pos')
           else (line, T.drop 1 rest', advance pos' '\n')

    consumeBlockComment p cs =
      let goBlock pos' src' =
            case T.uncons src' of
              Nothing -> Left (CompileError "unterminated block comment" (Just pos'.spLine) (Just pos'.spCol))
              Just (x, rest0) ->
                case T.uncons rest0 of
                  Just (y, rest)
                    | x == '*' && y == '/' -> Right (rest, advance (advance pos' x) y)
                    | otherwise -> goBlock (advance pos' x) rest0
                  Nothing -> goBlock (advance pos' x) T.empty
      in goBlock p cs

    consumeIdent p cs =
      let (ident, rest) = T.span (\x -> isAlphaNum x || x == '_') cs
          pos' = advanceCols p (T.length ident)
      in (ident, rest, pos')

    consumeNumber p cs =
      case T.stripPrefix "0x" cs <|> T.stripPrefix "0X" cs of
        Just restHex ->
          let (hexRaw, rest) = T.span isHexDigitOrUnderscore restHex
              hexLen = T.length hexRaw
              pos' = advanceCols p (2 + hexLen)
          in if T.all (== '_') hexRaw
              then (TkInt 0 Nothing, restHex, p)
              else
                let val = parseHexUnderscore hexRaw
                in applySuffix (TkInt val Nothing) rest pos'
        Nothing ->
          let (intRaw, rest0) = T.span isDigitOrUnderscore cs
              intVal = parseDecUnderscore intRaw
              intRawLen = T.length intRaw
          in case T.uncons rest0 of
              Just ('.', r) ->
                case T.uncons r of
                  Just (d, _rest1) | isDigit d ->
                    let (fracRaw, rest1) = T.span isDigitOrUnderscore r
                        fracDigits = removeUnderscores fracRaw
                        intDigits = removeUnderscores intRaw
                        (expTxt, expRaw, rest2, okExp) = parseExponent rest1
                        numTxt = intDigits <> "." <> fracDigits <> expTxt
                        consumedLen = intRawLen + 1 + T.length fracRaw + T.length expRaw
                        pos' = advanceCols p consumedLen
                    in if okExp
                        then applySuffix (TkFloat (parseFloatText numTxt) Nothing) rest2 pos'
                        else
                          let floatTxt = intDigits <> "." <> fracDigits
                              floatPos = advanceCols p (intRawLen + 1 + T.length fracRaw)
                          in applySuffix (TkFloat (parseFloatText floatTxt) Nothing) rest1 floatPos
                  _ ->
                    let (expTxt, expRaw, rest1, okExp) = parseExponent rest0
                        pos' = advanceCols p (intRawLen + T.length expRaw)
                    in if okExp
                        then
                          let intDigits = removeUnderscores intRaw
                              numTxt = intDigits <> expTxt
                          in applySuffix (TkFloat (parseFloatText numTxt) Nothing) rest1 pos'
                        else applySuffix (TkInt intVal Nothing) rest0 (advanceCols p intRawLen)
              _ ->
                let (expTxt, expRaw, rest1, okExp) = parseExponent rest0
                    pos' = advanceCols p (intRawLen + T.length expRaw)
                in if okExp
                    then
                      let intDigits = removeUnderscores intRaw
                          numTxt = intDigits <> expTxt
                      in applySuffix (TkFloat (parseFloatText numTxt) Nothing) rest1 pos'
                    else applySuffix (TkInt intVal Nothing) rest0 (advanceCols p intRawLen)
      where
        isDigitOrUnderscore c = isDigit c || c == '_'
        isHexDigitOrUnderscore c = isHexDigit c || c == '_'
        removeUnderscores = T.filter (/= '_')

        applySuffix tok rest pos' =
          case T.uncons rest of
            Just (s, rest1) ->
              case tok of
                TkInt n _ ->
                  case intSuffix s of
                    Just suf -> (TkInt n (Just suf), rest1, advance pos' s)
                    Nothing -> (TkInt n Nothing, rest, pos')
                TkFloat f _ ->
                  case floatSuffix s of
                    Just suf -> (TkFloat f (Just suf), rest1, advance pos' s)
                    Nothing -> (TkFloat f Nothing, rest, pos')
                _ -> (tok, rest, pos')
            _ -> (tok, rest, pos')

        intSuffix s =
          case s of
            'i' -> Just IntSuffixI
            'u' -> Just IntSuffixU
            _ -> Nothing

        floatSuffix s =
          case s of
            'f' -> Just FloatSuffixF
            'h' -> Just FloatSuffixH
            _ -> Nothing

        parseExponent rest =
          case T.uncons rest of
            Just (e, more) | e == 'e' || e == 'E' ->
              let (signTxt, rest1) =
                    case T.uncons more of
                      Just (s, restSign) | s == '+' || s == '-' -> (T.singleton s, restSign)
                      _ -> ("", more)
                  (expRaw, rest2) = T.span isDigitOrUnderscore rest1
                  expDigits = removeUnderscores expRaw
                  expHead = T.singleton e <> signTxt
              in if T.null expDigits
                  then ("", "", rest, False)
                  else (expHead <> expDigits, expHead <> expRaw, rest2, True)
            _ -> ("", "", rest, False)

        parseDecUnderscore = T.foldl' step 0
          where
            step acc ch
              | ch == '_' = acc
              | otherwise = acc * 10 + fromIntegral (digitToInt ch)

        parseHexUnderscore = T.foldl' step 0
          where
            step acc ch
              | ch == '_' = acc
              | otherwise = acc * 16 + fromIntegral (digitToInt ch)

        parseFloatText txt =
          case TR.double txt of
            Right (v, rest) | T.null rest -> realToFrac v
            _ -> 0

    consumeString p cs =
      let goStr acc pos' src' =
            case T.uncons src' of
              Nothing -> Left (CompileError "unterminated string" (Just pos'.spLine) (Just pos'.spCol))
              Just (x, xs)
                | x == '"' -> Right (T.pack (reverse acc), xs, advance pos' x)
                | x == '\\' ->
                    case T.uncons xs of
                      Nothing -> Left (CompileError "unterminated escape" (Just pos'.spLine) (Just pos'.spCol))
                      Just (e, rest) -> goStr (e : acc) (advance (advance pos' x) e) rest
                | otherwise -> goStr (x : acc) (advance pos' x) xs
      in goStr [] (advance p '"') cs

    advance (SrcPos l c) ch
      | ch == '\n' = SrcPos (l + 1) 1
      | otherwise = SrcPos l (c + 1)

    advanceCols (SrcPos l c) n = SrcPos l (c + n)


parseModuleTokensWith :: FeatureSet -> [Token] -> Either CompileError ModuleAst
parseModuleTokensWith feats toks =
  let loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries seenNonDirective seenNonImport rest =
        case rest of
          [] ->
            Right (ModuleAst (reverse accDirectives) (reverse accImports) (reverse accAliases) (reverse accStructs) (reverse accBindings) (reverse accGlobals) (reverse accConsts) (reverse accOverrides) (reverse accAsserts) (reverse accFns) (reverse accEntries))
          _ -> do
            (attrs, rest1) <- parseAttributes rest
            (keep, attrs') <- applyIf attrs feats rest1
            case rest1 of
              (Token (TkIdent "enable") _ : _) -> do
                when (seenNonDirective && keep) $
                  Left (errorAt rest1 "directives must appear before any declarations")
                unless (null attrs') $
                  Left (errorAt rest1 "enable does not accept attributes other than @if")
                (dir, rest2) <- parseEnableDirective rest1
                let accDirectives' = if keep then dir : accDirectives else accDirectives
                loop accDirectives' accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries seenNonDirective seenNonImport rest2
              (Token (TkIdent "diagnostic") _ : _) -> do
                when (seenNonDirective && keep) $
                  Left (errorAt rest1 "directives must appear before any declarations")
                unless (null attrs') $
                  Left (errorAt rest1 "diagnostic does not accept attributes other than @if")
                (dir, rest2) <- parseDiagnosticDirective rest1
                let accDirectives' = if keep then dir : accDirectives else accDirectives
                loop accDirectives' accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries seenNonDirective seenNonImport rest2
              (Token (TkIdent "import") _ : _) -> do
                when (seenNonImport && keep) $
                  Left (errorAt rest1 "imports must appear before other declarations")
                unless (null attrs') $
                  Left (errorAt rest1 "import does not accept attributes other than @if")
                (importDecl, rest2) <- parseImportStatement rest1
                let accImports' = if keep then importDecl : accImports else accImports
                loop accDirectives accImports' accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries True seenNonImport rest2
              (Token (TkIdent "alias") _ : _) -> do
                unless (null attrs') $
                  Left (errorAt rest1 "alias does not accept attributes other than @if")
                (aliasDecl, rest2) <- parseAliasDecl rest1
                let accAliases' = if keep then aliasDecl : accAliases else accAliases
                loop accDirectives accImports accAliases' accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries True True rest2
              (Token (TkIdent "const_assert") _ : _) -> do
                unless (null attrs') $
                  Left (errorAt rest1 "const_assert does not accept attributes other than @if")
                (assertDecl, rest2) <- parseConstAssert rest1
                let accAsserts' = if keep then assertDecl : accAsserts else accAsserts
                loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts' accFns accEntries True True rest2
              (Token (TkIdent "struct") _ : _) -> do
                (mStruct, rest2) <- parseStruct feats rest1
                let accStructs' = case (keep, mStruct) of
                      (True, Just structDecl) -> structDecl : accStructs
                      _ -> accStructs
                loop accDirectives accImports accAliases accStructs' accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries True True rest2
              (Token (TkIdent "var") _ : _) -> do
                let hasBindingAttrs = isJust (attrInt "group" attrs') || isJust (attrInt "binding" attrs')
                (addrSpace, _access, _rest) <- parseAddressSpaceMaybe (drop 1 rest1)
                let isBindingSpace = maybe False (`elem` ["uniform", "storage", "sampler", "texture"]) addrSpace
                if hasBindingAttrs || isBindingSpace
                  then do
                    (bindingDecl, rest2) <- parseBinding attrs' rest1
                    let accBindings' = if keep then bindingDecl : accBindings else accBindings
                    loop accDirectives accImports accAliases accStructs accBindings' accGlobals accConsts accOverrides accAsserts accFns accEntries True True rest2
                  else do
                    (globalDecl, rest2) <- parseGlobalVar attrs' rest1
                    let accGlobals' = if keep then globalDecl : accGlobals else accGlobals
                    loop accDirectives accImports accAliases accStructs accBindings accGlobals' accConsts accOverrides accAsserts accFns accEntries True True rest2
              (Token (TkIdent "let") _ : _) -> do
                (constDecl, rest2) <- parseConstDecl attrs' rest1
                let accConsts' = if keep then constDecl : accConsts else accConsts
                loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts' accOverrides accAsserts accFns accEntries True True rest2
              (Token (TkIdent "override") _ : _) -> do
                let idAttr =
                      case attrInt "id" attrs' of
                        Nothing -> Nothing
                        Just v ->
                          if v < 0 || v > fromIntegral (maxBound :: Word32)
                            then Nothing
                            else Just (fromIntegral v)
                when (maybe False (\v -> v < 0 || v > fromIntegral (maxBound :: Word32)) (attrInt "id" attrs')) $
                  Left (errorAt rest1 "@id must be a non-negative 32-bit integer")
                let otherAttrs = [a | a <- attrs', not (isIdAttr a)]
                unless (null otherAttrs) $
                  Left (errorAt rest1 "override only accepts @id and @if attributes")
                (overrideDecl, rest2) <- parseOverrideDecl idAttr rest1
                let accOverrides' = if keep then overrideDecl : accOverrides else accOverrides
                loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides' accAsserts accFns accEntries True True rest2
              (Token (TkIdent "fn") _ : _) -> do
                (name, params, retType, retLoc, retBuiltin, body, rest2) <- parseFunctionGeneric feats rest1
                let hasStageAttr = any isStageAttr attrs'
                case entryAttributesMaybe attrs' of
                  Nothing ->
                    if hasStageAttr
                      then Left (errorAt rest1 "invalid entry point attributes")
                      else do
                        case retLoc of
                          Nothing -> pure ()
                          Just _ -> Left (errorAt rest1 "return location is only allowed on entry points")
                        case retBuiltin of
                          Nothing -> pure ()
                          Just _ -> Left (errorAt rest1 "return builtin is only allowed on entry points")
                        let fnDecl = FunctionDecl name params retType body
                        let accFns' = if keep then fnDecl : accFns else accFns
                        loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns' accEntries True True rest2
                  Just (stage, workgroup) -> do
                    let entry = EntryPoint name stage workgroup params retType retLoc retBuiltin body
                    let accEntries' = if keep then entry : accEntries else accEntries
                    loop accDirectives accImports accAliases accStructs accBindings accGlobals accConsts accOverrides accAsserts accFns accEntries' True True rest2
              _ -> Left (errorAt rest1 "expected directive, import, alias, struct, var, let, override, const_assert, or fn")
  in loop [] [] [] [] [] [] [] [] [] [] [] False False toks
  where
    isStageAttr attr =
      case attr of
        Attr n _ -> n == "compute" || n == "fragment" || n == "vertex"
        AttrIf _ -> False
    isIdAttr attr =
      case attr of
        Attr n [AttrInt _] -> n == "id"
        _ -> False

parseAttributes :: [Token] -> Either CompileError ([Attr], [Token])
parseAttributes toks =
  let go acc rest =
        case rest of
          (Token (TkSymbol "@") _ : Token (TkIdent name) _ : more) ->
            if name == "if"
              then do
                (expr, rest') <- parseIfAttr more
                go (AttrIf expr : acc) rest'
              else do
                (args, rest') <- parseAttrArgs more
                go (Attr name args : acc) rest'
          _ -> Right (reverse acc, rest)
  in go [] toks

parseIfAttr :: [Token] -> Either CompileError (TTExpr, [Token])
parseIfAttr toks =
  case toks of
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseTTExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected @if(<expr>)")

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
      case parseConstExpr [",", ")"] rest of
        Right (expr, more) ->
          case expr of
            CEInt n -> Right (AttrInt n, more)
            CEIdent name -> Right (AttrIdent name, more)
            _ -> Right (AttrExpr expr, more)
        Left _ -> Left (errorAt rest "expected attribute argument")

parseConstExpr :: [Text] -> [Token] -> Either CompileError (ConstExpr, [Token])
parseConstExpr stop toks = parseConstBitOr toks
  where
    isStop rest =
      case rest of
        (Token (TkSymbol sym) _ : _) -> sym `elem` stop
        _ -> False

    parseConstBitOr ts = do
      (lhs, rest) <- parseConstBitXor ts
      parseConstBitOrTail lhs rest
    parseConstBitOrTail lhs rest
      | isStop rest = Right (lhs, rest)
      | otherwise =
          case rest of
            (Token (TkSymbol "|") _ : more) -> do
              (rhs, rest1) <- parseConstBitXor more
              parseConstBitOrTail (CEBinary CBitOr lhs rhs) rest1
            _ -> Right (lhs, rest)

    parseConstBitXor ts = do
      (lhs, rest) <- parseConstBitAnd ts
      parseConstBitXorTail lhs rest
    parseConstBitXorTail lhs rest
      | isStop rest = Right (lhs, rest)
      | otherwise =
          case rest of
            (Token (TkSymbol "^") _ : more) -> do
              (rhs, rest1) <- parseConstBitAnd more
              parseConstBitXorTail (CEBinary CBitXor lhs rhs) rest1
            _ -> Right (lhs, rest)

    parseConstBitAnd ts = do
      (lhs, rest) <- parseConstShift ts
      parseConstBitAndTail lhs rest
    parseConstBitAndTail lhs rest
      | isStop rest = Right (lhs, rest)
      | otherwise =
          case rest of
            (Token (TkSymbol "&") _ : more) -> do
              (rhs, rest1) <- parseConstShift more
              parseConstBitAndTail (CEBinary CBitAnd lhs rhs) rest1
            _ -> Right (lhs, rest)

    parseConstShift ts = do
      (lhs, rest) <- parseConstAddSub ts
      parseConstShiftTail lhs rest
    parseConstShiftTail lhs rest
      | isStop rest = Right (lhs, rest)
      | otherwise =
          case rest of
            (Token (TkSymbol "<<") _ : more) -> do
              (rhs, rest1) <- parseConstAddSub more
              parseConstShiftTail (CEBinary CShl lhs rhs) rest1
            (Token (TkSymbol ">>") _ : more) -> do
              (rhs, rest1) <- parseConstAddSub more
              parseConstShiftTail (CEBinary CShr lhs rhs) rest1
            _ -> Right (lhs, rest)

    parseConstAddSub ts = do
      (lhs, rest) <- parseConstMulDiv ts
      parseConstAddSubTail lhs rest
    parseConstAddSubTail lhs rest
      | isStop rest = Right (lhs, rest)
      | otherwise =
          case rest of
            (Token (TkSymbol "+") _ : more) -> do
              (rhs, rest1) <- parseConstMulDiv more
              parseConstAddSubTail (CEBinary CAdd lhs rhs) rest1
            (Token (TkSymbol "-") _ : more) -> do
              (rhs, rest1) <- parseConstMulDiv more
              parseConstAddSubTail (CEBinary CSub lhs rhs) rest1
            _ -> Right (lhs, rest)

    parseConstMulDiv ts = do
      (lhs, rest) <- parseConstUnary ts
      parseConstMulDivTail lhs rest
    parseConstMulDivTail lhs rest
      | isStop rest = Right (lhs, rest)
      | otherwise =
          case rest of
            (Token (TkSymbol "*") _ : more) -> do
              (rhs, rest1) <- parseConstUnary more
              parseConstMulDivTail (CEBinary CMul lhs rhs) rest1
            (Token (TkSymbol "/") _ : more) -> do
              (rhs, rest1) <- parseConstUnary more
              parseConstMulDivTail (CEBinary CDiv lhs rhs) rest1
            (Token (TkSymbol "%") _ : more) -> do
              (rhs, rest1) <- parseConstUnary more
              parseConstMulDivTail (CEBinary CMod lhs rhs) rest1
            _ -> Right (lhs, rest)

    parseConstUnary ts =
      case ts of
        (Token (TkSymbol "-") _ : more) -> do
          (expr, rest) <- parseConstUnary more
          Right (CEUnaryNeg expr, rest)
        _ -> parseConstPrimary ts

    parseConstPrimary ts =
      case ts of
        (Token (TkInt n mSuffix) _ : rest) ->
          Right (constIntExpr n mSuffix, rest)
        (Token (TkIdent name) _ : Token (TkSymbol "(") _ : more) -> do
          (args, rest1) <- parseConstCallArgs [] more
          Right (CECall name args, rest1)
        (Token (TkIdent name) _ : rest) -> Right (CEIdent name, rest)
        (Token (TkSymbol "(") _ : more) -> do
          (expr, rest1) <- parseConstExpr [")"] more
          rest2 <- expectSymbol ")" rest1
          Right (expr, rest2)
        _ -> Left (errorAt ts "expected constant expression")
      where
        constIntExpr n mSuffix =
          case mSuffix of
            Nothing -> CEInt n
            Just IntSuffixU -> CECall "u32" [CEInt n]
            Just IntSuffixI -> CECall "i32" [CEInt n]

    parseConstCallArgs acc ts =
      case ts of
        (Token (TkSymbol ")") _ : rest) -> Right (reverse acc, rest)
        _ -> do
          (expr, rest1) <- parseConstExpr [",", ")"] ts
          case rest1 of
            (Token (TkSymbol ",") _ : more) -> parseConstCallArgs (expr:acc) more
            (Token (TkSymbol ")") _ : more) -> Right (reverse (expr:acc), more)
            _ -> Left (errorAt rest1 "expected ',' or ')' in call arguments")

parseTTExpr :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTExpr = parseTTOr

parseTTOr :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTOr toks = do
  (lhs, rest) <- parseTTAnd toks
  parseTTOrTail lhs rest
  where
    parseTTOrTail lhs rest =
      case rest of
        (Token (TkSymbol "||") _ : more) -> do
          (rhs, rest1) <- parseTTAnd more
          parseTTOrTail (TTOr lhs rhs) rest1
        _ -> Right (lhs, rest)

parseTTAnd :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTAnd toks = do
  (lhs, rest) <- parseTTNot toks
  parseTTAndTail lhs rest
  where
    parseTTAndTail lhs rest =
      case rest of
        (Token (TkSymbol "&&") _ : more) -> do
          (rhs, rest1) <- parseTTNot more
          parseTTAndTail (TTAnd lhs rhs) rest1
        _ -> Right (lhs, rest)

parseTTNot :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTNot toks =
  case toks of
    (Token (TkSymbol "!") _ : more) -> do
      (expr, rest) <- parseTTNot more
      Right (TTNot expr, rest)
    _ -> parseTTPrimary toks

parseTTPrimary :: [Token] -> Either CompileError (TTExpr, [Token])
parseTTPrimary toks =
  case toks of
    (Token (TkIdent "true") _ : rest) -> Right (TTBool True, rest)
    (Token (TkIdent "false") _ : rest) -> Right (TTBool False, rest)
    (Token (TkIdent name) _ : rest) -> Right (TTVar name, rest)
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseTTExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected translate-time expression")

applyIf :: [Attr] -> FeatureSet -> [Token] -> Either CompileError (Bool, [Attr])
applyIf attrs feats _toks = do
  (ifExprs, attrs') <- stripIfAttr attrs
  Right (null ifExprs || all (evalTTExpr feats) ifExprs, attrs')

stripIfAttr :: [Attr] -> Either CompileError ([TTExpr], [Attr])
stripIfAttr attrs =
  let (ifs, others) = partition isIf attrs
  in Right ([expr | AttrIf expr <- ifs], others)
  where
    isIf attr =
      case attr of
        AttrIf _ -> True
        _ -> False

evalTTExpr :: FeatureSet -> TTExpr -> Bool
evalTTExpr feats expr =
  case expr of
    TTVar name -> Set.member name feats
    TTBool b -> b
    TTNot a -> not (evalTTExpr feats a)
    TTAnd a b -> evalTTExpr feats a && evalTTExpr feats b
    TTOr a b -> evalTTExpr feats a || evalTTExpr feats b

parseStruct :: FeatureSet -> [Token] -> Either CompileError (Maybe StructDecl, [Token])
parseStruct feats toks =
  case toks of
    (Token (TkIdent "struct") _ : Token (TkIdent name) _ : Token (TkSymbol "{") _ : rest) -> do
      (fields, rest1) <- parseStructFields feats [] rest
      case rest1 of
        (Token (TkSymbol ";") _ : more) -> Right (Just (StructDecl name fields), more)
        _ -> Right (Just (StructDecl name fields), rest1)
    _ -> Left (errorAt toks "malformed struct declaration")

parseStructFields :: FeatureSet -> [FieldDecl] -> [Token] -> Either CompileError ([FieldDecl], [Token])
parseStructFields feats acc rest =
  case rest of
    (Token (TkSymbol "}") _ : more) -> Right (reverse acc, more)
    _ -> do
      (mField, rest1) <- parseStructField feats rest
      let rest2 = case rest1 of
            (Token (TkSymbol ",") _ : more) -> more
            (Token (TkSymbol ";") _ : more) -> more
            _ -> rest1
      let acc' = case mField of
            Just field -> field : acc
            Nothing -> acc
      parseStructFields feats acc' rest2

parseStructField :: FeatureSet -> [Token] -> Either CompileError (Maybe FieldDecl, [Token])
parseStructField feats toks = do
  (attrs, rest1) <- parseAttributes toks
  (keep, attrs') <- applyIf attrs feats rest1
  case rest1 of
    (Token (TkIdent name) _ : Token (TkSymbol ":") _ : rest) -> do
      (ty, rest2) <- parseType rest
      let field = FieldDecl name ty attrs'
      Right (if keep then Just field else Nothing, rest2)
    _ -> Left (errorAt rest1 "expected struct field")

parseImportStatement :: [Token] -> Either CompileError (ImportDecl, [Token])
parseImportStatement toks =
  case toks of
    (Token (TkIdent "import") _ : rest) -> do
      (rel, rest1) <- parseImportRelative rest
      (items, rest2) <- parseImportClause rest1
      rest3 <- expectSymbol ";" rest2
      Right (ImportDecl rel items, rest3)
    _ -> Left (errorAt toks "expected import declaration")

parseImportRelative :: [Token] -> Either CompileError (Maybe ImportRelative, [Token])
parseImportRelative toks =
  case toks of
    (Token (TkIdent "package") _ : Token (TkSymbol "::") _ : rest) ->
      Right (Just ImportPackage, rest)
    (Token (TkIdent "super") _ : Token (TkSymbol "::") _ : rest) -> do
      (count, rest1) <- parseSuperChain 1 rest
      Right (Just (ImportSuper count), rest1)
    _ -> Right (Nothing, toks)
  where
    parseSuperChain count rest =
      case rest of
        (Token (TkIdent "super") _ : Token (TkSymbol "::") _ : more) ->
          parseSuperChain (count + 1) more
        _ -> Right (count, rest)

parseImportClause :: [Token] -> Either CompileError ([ImportItem], [Token])
parseImportClause toks =
  case toks of
    (Token (TkSymbol "{") _ : _) -> parseImportCollection toks
    _ -> parseImportPathOrItem toks

parseImportCollection :: [Token] -> Either CompileError ([ImportItem], [Token])
parseImportCollection toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> do
      (revItems, rest1) <- parseImportItems [] rest
      Right (reverse revItems, rest1)
    _ -> Left (errorAt toks "expected import collection")
  where
    parseImportItems acc rest =
      case rest of
        (Token (TkSymbol "}") _ : more) -> Right (acc, more)
        _ -> do
          (items, rest1) <- parseImportPathOrItem rest
          let acc' = reverse items <> acc
          case rest1 of
            (Token (TkSymbol ",") _ : more) -> parseImportItems acc' more
            (Token (TkSymbol "}") _ : more) -> Right (acc', more)
            _ -> Left (errorAt rest1 "expected ',' or '}' in import collection")

parseImportPathOrItem :: [Token] -> Either CompileError ([ImportItem], [Token])
parseImportPathOrItem toks = do
  (name, rest) <- parseIdent toks
  case rest of
    (Token (TkSymbol "::") _ : more) ->
      case more of
        (Token (TkSymbol "{") _ : _) -> do
          (items, rest1) <- parseImportCollection more
          Right (prefix name items, rest1)
        _ -> do
          (items, rest1) <- parseImportPathOrItem more
          Right (prefix name items, rest1)
    _ -> do
      (alias, rest1) <- parseImportAlias rest
      Right ([ImportItem [name] alias], rest1)
  where
    prefix name items = [item { iiPath = name : item.iiPath } | item <- items]

parseImportAlias :: [Token] -> Either CompileError (Maybe Text, [Token])
parseImportAlias toks =
  case toks of
    (Token (TkIdent "as") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      Right (Just name, rest1)
    _ -> Right (Nothing, toks)

parseBinding :: [Attr] -> [Token] -> Either CompileError (BindingDecl, [Token])
parseBinding attrs toks =
  case toks of
    (Token (TkIdent "var") _ : rest) -> do
      (addrSpace, access, rest1) <- parseAddressSpaceMaybe rest
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      rest5 <- expectSymbol ";" rest4
      (group, bindingIx) <- bindingNumbers attrs
      kind <- case addrSpace of
        Just space -> toBindingKind space access ty
        Nothing -> bindingKindFromType ty
      Right (BindingDecl name kind group bindingIx ty, rest5)
    _ -> Left (errorAt toks "expected var declaration")

parseGlobalVar :: [Attr] -> [Token] -> Either CompileError (GlobalVarDecl, [Token])
parseGlobalVar attrs toks = do
  unless (null attrs) $
    Left (errorAt toks "global variables do not accept attributes other than @if")
  case toks of
    (Token (TkIdent "var") _ : rest) -> do
      (addrSpace, access, rest1) <- parseAddressSpaceMaybe rest
      space <- case addrSpace of
        Just s -> Right s
        Nothing -> Left (errorAt rest "global variables require an explicit address space (e.g. var<private>)")
      when (isJust access) $
        Left (errorAt rest "global variables do not support access qualifiers")
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      (initExpr, rest5) <- parseGlobalVarInit rest4
      rest6 <- expectSymbol ";" rest5
      Right (GlobalVarDecl name space ty initExpr, rest6)
    _ -> Left (errorAt toks "expected var declaration")
  where
    parseGlobalVarInit rest =
      case rest of
        (Token (TkSymbol "=") _ : more) -> do
          (expr, rest1) <- parseExpr more
          Right (Just expr, rest1)
        _ -> Right (Nothing, rest)

parseConstDecl :: [Attr] -> [Token] -> Either CompileError (ConstDecl, [Token])
parseConstDecl attrs toks = do
  unless (null attrs) $
    Left (errorAt toks "const declarations do not accept attributes other than @if")
  case toks of
    (Token (TkIdent kw) _ : rest) | kw == "let" || kw == "const" -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseTypeAnnotationMaybe rest1
      rest3 <- expectSymbol "=" rest2
      (expr, rest4) <- parseExpr rest3
      rest5 <- expectSymbol ";" rest4
      Right (ConstDecl name mType expr, rest5)
    _ -> Left (errorAt toks "expected let declaration")

parseAliasDecl :: [Token] -> Either CompileError (AliasDecl, [Token])
parseAliasDecl toks =
  case toks of
    (Token (TkIdent "alias") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      rest2 <- expectSymbol "=" rest1
      (ty, rest3) <- parseType rest2
      rest4 <- expectSymbol ";" rest3
      Right (AliasDecl name ty, rest4)
    _ -> Left (errorAt toks "expected alias declaration")

parseOverrideDecl :: Maybe Word32 -> [Token] -> Either CompileError (OverrideDecl, [Token])
parseOverrideDecl overrideId toks =
  case toks of
    (Token (TkIdent "override") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseOverrideTypeMaybe rest1
      (mExpr, rest3) <- parseOverrideInitMaybe rest2
      rest4 <- expectSymbol ";" rest3
      case (mType, mExpr) of
        (Nothing, Nothing) ->
          Left (errorAt rest1 "override declarations require a type or initializer")
        _ ->
          Right (OverrideDecl name overrideId mType mExpr, rest4)
    _ -> Left (errorAt toks "expected override declaration")
  where
    parseOverrideTypeMaybe rest =
      case rest of
        (Token (TkSymbol ":") _ : more) -> do
          (ty, rest1) <- parseType more
          Right (Just ty, rest1)
        _ -> Right (Nothing, rest)

    parseOverrideInitMaybe rest =
      case rest of
        (Token (TkSymbol "=") _ : more) -> do
          (expr, rest1) <- parseExpr more
          Right (Just expr, rest1)
        _ -> Right (Nothing, rest)

parseConstAssert :: [Token] -> Either CompileError (ConstAssert, [Token])
parseConstAssert toks =
  case toks of
    (Token (TkIdent "const_assert") pos : rest) -> do
      rest1 <- expectSymbol "(" rest
      (expr, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      rest4 <- expectSymbol ";" rest3
      Right (ConstAssert pos expr, rest4)
    _ -> Left (errorAt toks "expected const_assert")

parseEnableDirective :: [Token] -> Either CompileError (Directive, [Token])
parseEnableDirective toks =
  case toks of
    (Token (TkIdent "enable") _ : rest) -> do
      (name, rest1) <- parseFullIdent rest
      rest2 <- expectSymbol ";" rest1
      Right (DirEnable name, rest2)
    _ -> Left (errorAt toks "expected enable directive")

parseDiagnosticDirective :: [Token] -> Either CompileError (Directive, [Token])
parseDiagnosticDirective toks =
  case toks of
    (Token (TkIdent "diagnostic") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (severityName, rest2) <- parseIdent rest1
      rest3 <- expectSymbol "," rest2
      (ruleName, rest4) <- parseFullIdent rest3
      rest5 <- expectSymbol ")" rest4
      rest6 <- expectSymbol ";" rest5
      severity <- parseDiagnosticSeverity severityName rest1
      Right (DirDiagnostic severity ruleName, rest6)
    _ -> Left (errorAt toks "expected diagnostic directive")
  where
    parseDiagnosticSeverity name toks' =
      case name of
        "error" -> Right DiagError
        "warning" -> Right DiagWarning
        "info" -> Right DiagInfo
        "off" -> Right DiagOff
        _ -> Left (errorAt toks' "unknown diagnostic severity")

parseAddressSpaceMaybe :: [Token] -> Either CompileError (Maybe Text, Maybe Text, [Token])
parseAddressSpaceMaybe toks =
  case toks of
    (Token (TkSymbol "<") _ : _) -> do
      (addr, access, rest) <- parseAddressSpace toks
      Right (Just addr, access, rest)
    _ -> Right (Nothing, Nothing, toks)

parseAddressSpace :: [Token] -> Either CompileError (Text, Maybe Text, [Token])
parseAddressSpace toks =
  case toks of
    (Token (TkSymbol "<") _ : Token (TkIdent addr) _ : rest) ->
      case rest of
        (Token (TkSymbol ",") _ : Token (TkIdent access) _ : Token (TkSymbol ">") _ : more) ->
          Right (addr, Just access, more)
        (Token (TkSymbol ">") _ : more) -> Right (addr, Nothing, more)
        _ -> Left (errorAt rest "expected address space qualifier")
    _ -> Left (errorAt toks "expected address space qualifiers")

parseFunctionGeneric :: FeatureSet -> [Token] -> Either CompileError (Text, [Param], Maybe Type, Maybe Word32, Maybe Text, [Stmt], [Token])
parseFunctionGeneric feats toks =
  case toks of
    (Token (TkIdent "fn") _ : Token (TkIdent name) _ : Token (TkSymbol "(") _ : rest) -> do
      (params, rest1) <- parseParams feats rest
      rest2 <- expectSymbol ")" rest1
      (rest3, retType, retLoc, retBuiltin) <- parseReturnType rest2
      (body, rest4) <- parseBody feats rest3
      Right (name, params, retType, retLoc, retBuiltin, body, rest4)
    _ -> Left (errorAt toks "expected fn declaration")

parseParams :: FeatureSet -> [Token] -> Either CompileError ([Param], [Token])
parseParams feats toks =
  case toks of
    (Token (TkSymbol ")") _ : _) -> Right ([], toks)
    _ -> parseParamList [] toks
  where
    parseParamList acc rest = do
      (mParam, rest1) <- parseParam rest
      let acc' = maybe acc (: acc) mParam
      case rest1 of
        (Token (TkSymbol ",") _ : more) -> parseParamList acc' more
        _ -> Right (reverse acc', rest1)

    parseParam rest = do
      (attrs, rest1) <- parseAttributes rest
      (keep, attrs') <- applyIf attrs feats rest1
      let pos = posOf rest1
      (name, rest2) <- parseIdent rest1
      rest3 <- expectSymbol ":" rest2
      (ty, rest4) <- parseType rest3
      let param = Param pos name ty attrs'
      pure (if keep then Just param else Nothing, rest4)

parseReturnType :: [Token] -> Either CompileError ([Token], Maybe Type, Maybe Word32, Maybe Text)
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

parseBody :: FeatureSet -> [Token] -> Either CompileError ([Stmt], [Token])
parseBody feats toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseStatements feats [] rest
    _ -> Left (errorAt toks "expected function body")

parseStatements :: FeatureSet -> [Stmt] -> [Token] -> Either CompileError ([Stmt], [Token])
parseStatements feats acc toks =
  case toks of
    (Token (TkSymbol "}") _ : more) -> Right (reverse acc, more)
    _ -> do
      (attrs, rest1) <- parseAttributes toks
      (keep, _attrs') <- applyIf attrs feats rest1
      (stmt, rest) <- parseStmt feats rest1
      let acc' = if keep then stmt : acc else acc
      parseStatements feats acc' rest

parseStmt :: FeatureSet -> [Token] -> Either CompileError (Stmt, [Token])
parseStmt feats toks =
  let pos = posOf toks
  in case toks of
    (Token (TkSymbol "++") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      rest2 <- expectSymbol ";" rest1
      Right (SInc pos lv, rest2)
    (Token (TkSymbol "--") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      rest2 <- expectSymbol ";" rest1
      Right (SDec pos lv, rest2)
    (Token (TkIdent "if") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (cond, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      (thenBody, rest4) <- parseBody feats rest3
      case rest4 of
        (Token (TkIdent "else") _ : more) -> do
          (elseBody, rest5) <- parseBody feats more
          Right (SIf pos cond thenBody (Just elseBody), rest5)
        _ -> Right (SIf pos cond thenBody Nothing, rest4)
    (Token (TkIdent "switch") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (expr, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      (cases, defBody, rest4) <- parseSwitchBody feats rest3
      Right (SSwitch pos expr cases defBody, rest4)
    (Token (TkIdent "loop") _ : rest) -> do
      (body, continuing, rest1) <- parseLoopBody feats rest
      Right (SLoop pos body continuing, rest1)
    (Token (TkIdent "while") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (cond, rest2) <- parseExpr rest1
      rest3 <- expectSymbol ")" rest2
      (body, rest4) <- parseBody feats rest3
      Right (SWhile pos cond body, rest4)
    (Token (TkIdent "for") _ : rest) -> do
      rest1 <- expectSymbol "(" rest
      (initStmt, rest2) <- parseForClause rest1
      rest3 <- expectSymbol ";" rest2
      (condExpr, rest4) <- parseForCond rest3
      rest5 <- expectSymbol ";" rest4
      (contStmt, rest6) <- parseForClause rest5
      rest7 <- expectSymbol ")" rest6
      (body, rest8) <- parseBody feats rest7
      Right (SFor pos initStmt condExpr contStmt body, rest8)
    (Token (TkIdent "break") _ : rest) ->
      case rest of
        (Token (TkIdent "if") _ : more) -> do
          rest1 <- expectSymbol "(" more
          (cond, rest2) <- parseExpr rest1
          rest3 <- expectSymbol ")" rest2
          rest4 <- expectSymbol ";" rest3
          Right (SBreakIf pos cond, rest4)
        _ -> do
          rest1 <- expectSymbol ";" rest
          Right (SBreak pos, rest1)
    (Token (TkIdent "continue") _ : rest) -> do
      rest1 <- expectSymbol ";" rest
      Right (SContinue pos, rest1)
    (Token (TkIdent "discard") _ : rest) -> do
      rest1 <- expectSymbol ";" rest
      Right (SDiscard pos, rest1)
    (Token (TkIdent "fallthrough") _ : rest) -> do
      rest1 <- expectSymbol ";" rest
      Right (SFallthrough pos, rest1)
    (Token (TkIdent "let") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseTypeAnnotationMaybe rest1
      rest3 <- expectSymbol "=" rest2
      (expr, rest4) <- parseExpr rest3
      rest5 <- expectSymbol ";" rest4
      Right (SLet pos name mType expr, rest5)
    (Token (TkIdent "var") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseTypeAnnotationMaybe rest1
      case rest2 of
        (Token (TkSymbol "=") _ : more) -> do
          (expr, rest3) <- parseExpr more
          rest4 <- expectSymbol ";" rest3
          Right (SVar pos name mType (Just expr), rest4)
        (Token (TkSymbol ";") _ : more) ->
          case mType of
            Nothing -> Left (errorAt rest2 "var declaration requires a type or initializer")
            Just _ -> Right (SVar pos name mType Nothing, more)
        _ -> Left (errorAt rest2 "expected '=' or ';' in var declaration")
    (Token (TkIdent "return") _ : rest) ->
      case rest of
        (Token (TkSymbol ";") _ : more) -> Right (SReturn pos Nothing, more)
        _ -> do
          (expr, rest1) <- parseExpr rest
          rest2 <- expectSymbol ";" rest1
          Right (SReturn pos (Just expr), rest2)
    _ -> do
      (lv, rest1) <- parseLValue toks
      case rest1 of
        (Token (TkSymbol "++") _ : restInc) -> do
          rest2 <- expectSymbol ";" restInc
          Right (SInc pos lv, rest2)
        (Token (TkSymbol "--") _ : restDec) -> do
          rest2 <- expectSymbol ";" restDec
          Right (SDec pos lv, rest2)
        (Token (TkSymbol "=") _ : restEq) -> do
          (expr, rest2) <- parseExpr restEq
          rest3 <- expectSymbol ";" rest2
          Right (SAssign pos lv expr, rest3)
        (Token (TkSymbol sym) _ : restEq)
          | Just op <- assignOpFromSymbol sym -> do
              (expr, rest2) <- parseExpr restEq
              rest3 <- expectSymbol ";" rest2
              Right (SAssignOp pos lv op expr, rest3)
        _ -> do
          (expr, rest2) <- parseExpr toks
          rest3 <- expectSymbol ";" rest2
          Right (SExpr pos expr, rest3)

parseTypeAnnotationMaybe :: [Token] -> Either CompileError (Maybe Type, [Token])
parseTypeAnnotationMaybe toks =
  case toks of
    (Token (TkSymbol ":") _ : rest) -> do
      (ty, rest1) <- parseType rest
      Right (Just ty, rest1)
    _ -> Right (Nothing, toks)

parseSwitchBody :: FeatureSet -> [Token] -> Either CompileError ([SwitchCase], Maybe [Stmt], [Token])
parseSwitchBody feats toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseCases [] Nothing rest
    _ -> Left (errorAt toks "expected '{' after switch")
  where
    parseCases acc def rest =
      case rest of
        (Token (TkSymbol "}") _ : more) -> Right (reverse acc, def, more)
        _ -> do
          (attrs, rest1) <- parseAttributes rest
          (keep, _attrs) <- applyIf attrs feats rest1
          case rest1 of
            (Token (TkIdent "case") _ : more) -> do
              (selectors, rest2) <- parseCaseSelectors more
              rest3 <- expectSymbol ":" rest2
              (body, rest4) <- parseBody feats rest3
              let acc' = if keep then SwitchCase selectors body : acc else acc
              parseCases acc' def rest4
            (Token (TkIdent "default") _ : more) -> do
              rest2 <- expectSymbol ":" more
              (body, rest3) <- parseBody feats rest2
              case def of
                Just _ -> Left (errorAt rest1 "multiple default cases in switch")
                Nothing -> parseCases acc (if keep then Just body else Nothing) rest3
            _ -> Left (errorAt rest1 "expected case or default in switch")

    parseCaseSelectors rest = do
      (expr, rest1) <- parseExpr rest
      parseMore [expr] rest1

    parseMore acc rest =
      case rest of
        (Token (TkSymbol ",") _ : more) -> do
          (expr, rest1) <- parseExpr more
          parseMore (acc <> [expr]) rest1
        _ -> Right (acc, rest)

parseLoopBody :: FeatureSet -> [Token] -> Either CompileError ([Stmt], Maybe [Stmt], [Token])
parseLoopBody feats toks =
  case toks of
    (Token (TkSymbol "{") _ : rest) -> parseLoopStmts [] rest
    _ -> Left (errorAt toks "expected '{' after loop")
  where
    parseLoopStmts acc rest =
      case rest of
        (Token (TkSymbol "}") _ : more) -> Right (reverse acc, Nothing, more)
        _ -> do
          (attrs, rest1) <- parseAttributes rest
          (keep, _attrs) <- applyIf attrs feats rest1
          case rest1 of
            (Token (TkIdent "continuing") _ : more) -> do
              (contBody, rest2) <- parseBody feats more
              case rest2 of
                (Token (TkSymbol "}") _ : more2) ->
                  Right (reverse acc, if keep then Just contBody else Nothing, more2)
                _ -> Left (errorAt rest2 "expected '}' after continuing block")
            _ -> do
              (stmt, rest2) <- parseStmt feats rest1
              let acc' = if keep then stmt : acc else acc
              parseLoopStmts acc' rest2

parseForClause :: [Token] -> Either CompileError (Maybe Stmt, [Token])
parseForClause toks =
  let pos = posOf toks
  in case toks of
    (Token (TkSymbol ";") _ : _) -> Right (Nothing, toks)
    (Token (TkSymbol ")") _ : _) -> Right (Nothing, toks)
    (Token (TkSymbol "++") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      Right (Just (SInc pos lv), rest1)
    (Token (TkSymbol "--") _ : rest) -> do
      (lv, rest1) <- parseLValue rest
      Right (Just (SDec pos lv), rest1)
    (Token (TkIdent "let") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseTypeAnnotationMaybe rest1
      rest3 <- expectSymbol "=" rest2
      (expr, rest4) <- parseExpr rest3
      Right (Just (SLet pos name mType expr), rest4)
    (Token (TkIdent "var") _ : rest) -> do
      (name, rest1) <- parseIdent rest
      (mType, rest2) <- parseTypeAnnotationMaybe rest1
      case rest2 of
        (Token (TkSymbol "=") _ : more) -> do
          (expr, rest3) <- parseExpr more
          Right (Just (SVar pos name mType (Just expr)), rest3)
        _ ->
          case mType of
            Nothing -> Left (errorAt rest2 "var declaration requires a type or initializer")
            Just _ -> Right (Just (SVar pos name mType Nothing), rest2)
    _ -> do
      (lv, rest1) <- parseLValue toks
      case rest1 of
        (Token (TkSymbol "++") _ : restInc) -> Right (Just (SInc pos lv), restInc)
        (Token (TkSymbol "--") _ : restDec) -> Right (Just (SDec pos lv), restDec)
        (Token (TkSymbol "=") _ : restEq) -> do
          (expr, rest3) <- parseExpr restEq
          Right (Just (SAssign pos lv expr), rest3)
        (Token (TkSymbol sym) _ : restEq)
          | Just op <- assignOpFromSymbol sym -> do
              (expr, rest3) <- parseExpr restEq
              Right (Just (SAssignOp pos lv op expr), rest3)
        _ -> Left (errorAt rest1 "expected assignment in for clause")

parseForCond :: [Token] -> Either CompileError (Maybe Expr, [Token])
parseForCond toks =
  case toks of
    (Token (TkSymbol ";") _ : _) -> Right (Nothing, toks)
    _ -> do
      (expr, rest) <- parseExpr toks
      Right (Just expr, rest)

assignOpFromSymbol :: Text -> Maybe BinOp
assignOpFromSymbol sym =
  case sym of
    "+=" -> Just OpAdd
    "-=" -> Just OpSub
    "*=" -> Just OpMul
    "/=" -> Just OpDiv
    "%=" -> Just OpMod
    "&=" -> Just OpBitAnd
    "|=" -> Just OpBitOr
    "^=" -> Just OpBitXor
    "<<=" -> Just OpShl
    ">>=" -> Just OpShr
    _ -> Nothing

parseLValue :: [Token] -> Either CompileError (LValue, [Token])
parseLValue toks =
  case toks of
    (Token (TkSymbol "*") pos : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (LVDeref pos expr, rest1)
    _ -> do
      let pos = posOf toks
      (name, rest) <- parseFullIdent toks
      parseLValueTail (LVVar pos name) rest

parseLValueTail :: LValue -> [Token] -> Either CompileError (LValue, [Token])
parseLValueTail lv toks =
  case toks of
    (Token (TkSymbol ".") pos : Token (TkIdent field) _ : rest) ->
      parseLValueTail (LVField pos lv field) rest
    (Token (TkSymbol "[") pos : rest) -> do
      (idx, rest1) <- parseExpr rest
      rest2 <- expectSymbol "]" rest1
      parseLValueTail (LVIndex pos lv idx) rest2
    _ -> Right (lv, toks)

parseExpr :: [Token] -> Either CompileError (Expr, [Token])
parseExpr = parseLogicalOr

parseLogicalOr :: [Token] -> Either CompileError (Expr, [Token])
parseLogicalOr toks = do
  (lhs, rest) <- parseLogicalAnd toks
  parseLogicalOrTail lhs rest
  where
    parseLogicalOrTail lhs toks' =
      case toks' of
        (Token (TkSymbol "||") pos : rest) -> do
          (rhs, rest1) <- parseLogicalAnd rest
          parseLogicalOrTail (EBinary pos OpOr lhs rhs) rest1
        _ -> Right (lhs, toks')

parseLogicalAnd :: [Token] -> Either CompileError (Expr, [Token])
parseLogicalAnd toks = do
  (lhs, rest) <- parseBitOr toks
  parseLogicalAndTail lhs rest
  where
    parseLogicalAndTail lhs toks' =
      case toks' of
        (Token (TkSymbol "&&") pos : rest) -> do
          (rhs, rest1) <- parseBitOr rest
          parseLogicalAndTail (EBinary pos OpAnd lhs rhs) rest1
        _ -> Right (lhs, toks')

parseBitOr :: [Token] -> Either CompileError (Expr, [Token])
parseBitOr toks = do
  (lhs, rest) <- parseBitXor toks
  parseBitOrTail lhs rest
  where
    parseBitOrTail lhs toks' =
      case toks' of
        (Token (TkSymbol "|") pos : rest) -> do
          (rhs, rest1) <- parseBitXor rest
          parseBitOrTail (EBinary pos OpBitOr lhs rhs) rest1
        _ -> Right (lhs, toks')

parseBitXor :: [Token] -> Either CompileError (Expr, [Token])
parseBitXor toks = do
  (lhs, rest) <- parseBitAnd toks
  parseBitXorTail lhs rest
  where
    parseBitXorTail lhs toks' =
      case toks' of
        (Token (TkSymbol "^") pos : rest) -> do
          (rhs, rest1) <- parseBitAnd rest
          parseBitXorTail (EBinary pos OpBitXor lhs rhs) rest1
        _ -> Right (lhs, toks')

parseBitAnd :: [Token] -> Either CompileError (Expr, [Token])
parseBitAnd toks = do
  (lhs, rest) <- parseEquality toks
  parseBitAndTail lhs rest
  where
    parseBitAndTail lhs toks' =
      case toks' of
        (Token (TkSymbol "&") pos : rest) -> do
          (rhs, rest1) <- parseEquality rest
          parseBitAndTail (EBinary pos OpBitAnd lhs rhs) rest1
        _ -> Right (lhs, toks')

parseEquality :: [Token] -> Either CompileError (Expr, [Token])
parseEquality toks = do
  (lhs, rest) <- parseRelational toks
  parseEqualityTail lhs rest
  where
    parseEqualityTail lhs toks' =
      case toks' of
        (Token (TkSymbol "==") pos : rest) -> do
          (rhs, rest1) <- parseRelational rest
          parseEqualityTail (EBinary pos OpEq lhs rhs) rest1
        (Token (TkSymbol "!=") pos : rest) -> do
          (rhs, rest1) <- parseRelational rest
          parseEqualityTail (EBinary pos OpNe lhs rhs) rest1
        _ -> Right (lhs, toks')

parseRelational :: [Token] -> Either CompileError (Expr, [Token])
parseRelational toks = do
  (lhs, rest) <- parseShift toks
  parseRelationalTail lhs rest
  where
    parseRelationalTail lhs toks' =
      case toks' of
        (Token (TkSymbol "<") pos : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary pos OpLt lhs rhs) rest1
        (Token (TkSymbol "<=") pos : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary pos OpLe lhs rhs) rest1
        (Token (TkSymbol ">") pos : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary pos OpGt lhs rhs) rest1
        (Token (TkSymbol ">=") pos : rest) -> do
          (rhs, rest1) <- parseShift rest
          parseRelationalTail (EBinary pos OpGe lhs rhs) rest1
        _ -> Right (lhs, toks')

parseShift :: [Token] -> Either CompileError (Expr, [Token])
parseShift toks = do
  (lhs, rest) <- parseAddSub toks
  parseShiftTail lhs rest
  where
    parseShiftTail lhs toks' =
      case toks' of
        (Token (TkSymbol "<<") pos : rest) -> do
          (rhs, rest1) <- parseAddSub rest
          parseShiftTail (EBinary pos OpShl lhs rhs) rest1
        (Token (TkSymbol ">>") pos : rest) -> do
          (rhs, rest1) <- parseAddSub rest
          parseShiftTail (EBinary pos OpShr lhs rhs) rest1
        _ -> Right (lhs, toks')

parseAddSub :: [Token] -> Either CompileError (Expr, [Token])
parseAddSub toks = do
  (lhs, rest) <- parseMulDiv toks
  parseAddSubTail lhs rest
  where
    parseAddSubTail lhs toks' =
      case toks' of
        (Token (TkSymbol "+") pos : rest) -> do
          (rhs, rest1) <- parseMulDiv rest
          parseAddSubTail (EBinary pos OpAdd lhs rhs) rest1
        (Token (TkSymbol "-") pos : rest) -> do
          (rhs, rest1) <- parseMulDiv rest
          parseAddSubTail (EBinary pos OpSub lhs rhs) rest1
        _ -> Right (lhs, toks')

parseMulDiv :: [Token] -> Either CompileError (Expr, [Token])
parseMulDiv toks = do
  (lhs, rest) <- parseUnaryExpr toks
  parseMulDivTail lhs rest
  where
    parseMulDivTail lhs toks' =
      case toks' of
        (Token (TkSymbol "*") pos : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary pos OpMul lhs rhs) rest1
        (Token (TkSymbol "/") pos : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary pos OpDiv lhs rhs) rest1
        (Token (TkSymbol "%") pos : rest) -> do
          (rhs, rest1) <- parseUnaryExpr rest
          parseMulDivTail (EBinary pos OpMod lhs rhs) rest1
        _ -> Right (lhs, toks')

parseUnaryExpr :: [Token] -> Either CompileError (Expr, [Token])
parseUnaryExpr toks =
  case toks of
    (Token (TkSymbol "-") pos : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary pos OpNeg expr, rest1)
    (Token (TkSymbol "!") pos : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary pos OpNot expr, rest1)
    (Token (TkSymbol "&") pos : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary pos OpAddr expr, rest1)
    (Token (TkSymbol "*") pos : rest) -> do
      (expr, rest1) <- parseUnaryExpr rest
      Right (EUnary pos OpDeref expr, rest1)
    _ -> parsePostfixExpr toks

parsePostfixExpr :: [Token] -> Either CompileError (Expr, [Token])
parsePostfixExpr toks = do
  (base, rest) <- parsePrimaryExpr toks
  parsePostfixTail base rest
  where
    parsePostfixTail expr toks' =
      case toks' of
        (Token (TkSymbol ".") pos : Token (TkIdent field) _ : rest) ->
          parsePostfixTail (EField pos expr field) rest
        (Token (TkSymbol "[") pos : rest) -> do
          (idx, rest1) <- parseExpr rest
          rest2 <- expectSymbol "]" rest1
          parsePostfixTail (EIndex pos expr idx) rest2
        _ -> Right (expr, toks')

parsePrimaryExpr :: [Token] -> Either CompileError (Expr, [Token])
parsePrimaryExpr toks =
  case toks of
    (Token (TkInt n mSuffix) pos : rest) -> Right (intLiteralExpr pos n mSuffix, rest)
    (Token (TkFloat f mSuffix) pos : rest) -> Right (floatLiteralExpr pos f mSuffix, rest)
    (Token (TkIdent "true") pos : rest) -> Right (EBool pos True, rest)
    (Token (TkIdent "false") pos : rest) -> Right (EBool pos False, rest)
    (Token (TkIdent _) _ : _) -> do
      let pos = posOf toks
      (name, rest) <- parseFullIdent toks
      case rest of
        (Token (TkSymbol "<") _ : _)
          | name == "bitcast" -> do
              rest1 <- expectSymbol "<" rest
              (targetTy, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              rest4 <- expectSymbol "(" rest3
              (args, rest5) <- parseCallArgs [] rest4
              case args of
                [arg] -> Right (EBitcast pos targetTy arg, rest5)
                _ -> Left (errorAt rest4 "bitcast expects one argument")
        (Token (TkSymbol "<") _ : _)
          | name `elem` ["vec2", "vec3", "vec4"] || isJust (parseMatrixName name) -> do
              let isVec = name `elem` ["vec2", "vec3", "vec4"]
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              rest4 <- expectSymbol "(" rest3
              (args, rest5) <- parseCallArgs [] rest4
              scalar <- case inner of
                TyScalar s -> Right s
                _ ->
                  Left
                    (errorAt rest1 (if isVec then "vector element must be a scalar" else "matrix element must be a scalar"))
              let name' = name <> "<" <> scalarName scalar <> ">"
              Right (ECall pos name' args, rest5)
        (Token (TkSymbol "(") _ : more) -> do
          (args, rest1) <- parseCallArgs [] more
          Right (ECall pos name args, rest1)
        _ -> Right (EVar pos name, rest)
    (Token (TkSymbol "(") _ : rest) -> do
      (expr, rest1) <- parseExpr rest
      rest2 <- expectSymbol ")" rest1
      Right (expr, rest2)
    _ -> Left (errorAt toks "expected expression")
  where
    intLiteralExpr pos n mSuffix =
      case mSuffix of
        Nothing -> EInt pos n
        Just IntSuffixU -> ECall pos "u32" [EInt pos n]
        Just IntSuffixI -> ECall pos "i32" [EInt pos n]
    floatLiteralExpr pos f mSuffix =
      case mSuffix of
        Nothing -> EFloat pos f
        Just FloatSuffixF -> ECall pos "f32" [EFloat pos f]
        Just FloatSuffixH -> ECall pos "f16" [EFloat pos f]

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

scalarName :: Scalar -> Text
scalarName scalar =
  case scalar of
    F16 -> "f16"
    F32 -> "f32"
    I32 -> "i32"
    U32 -> "u32"
    Bool -> "bool"

-- Shared parse helpers

parseType :: [Token] -> Either CompileError (Type, [Token])
parseType toks = do
  (name, rest) <- parseFullIdent toks
  if "::" `T.isInfixOf` name
    then Right (TyStructRef name, rest)
    else
      case name of
        _ | name `elem` ["i32", "u32", "f16", "f32", "bool"] ->
              Right (TyScalar (parseScalar name), rest)
        "sampler" ->
              Right (TySampler, rest)
        "sampler_comparison" ->
              Right (TySamplerComparison, rest)
        "ref" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              Right (TyPtr "function" Nothing inner, rest3)
        "ptr" -> do
              rest1 <- expectSymbol "<" rest
              (addrName, rest2) <- parseIdent rest1
              rest3 <- expectSymbol "," rest2
              (inner, rest4) <- parseType rest3
              (access, rest5) <- parsePtrAccessMaybe rest4
              rest6 <- expectSymbol ">" rest5
              Right (TyPtr addrName access inner, rest6)
        "atomic" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar ->
                  case scalar of
                    I32 -> Right (TyAtomic scalar, rest3)
                    U32 -> Right (TyAtomic scalar, rest3)
                    _ -> Left (errorAt rest "atomic element must be i32 or u32")
                _ -> Left (errorAt rest "atomic element must be i32 or u32")
        "texture_1d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture1D scalar, rest3)
                _ -> Left (errorAt rest "texture_1d element must be a scalar")
        "texture_1d_array" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture1DArray scalar, rest3)
                _ -> Left (errorAt rest "texture_1d_array element must be a scalar")
        "texture_2d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture2D scalar, rest3)
                _ -> Left (errorAt rest "texture_2d element must be a scalar")
        "texture_2d_array" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture2DArray scalar, rest3)
                _ -> Left (errorAt rest "texture_2d_array element must be a scalar")
        "texture_3d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTexture3D scalar, rest3)
                _ -> Left (errorAt rest "texture_3d element must be a scalar")
        "texture_cube" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTextureCube scalar, rest3)
                _ -> Left (errorAt rest "texture_cube element must be a scalar")
        "texture_cube_array" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTextureCubeArray scalar, rest3)
                _ -> Left (errorAt rest "texture_cube_array element must be a scalar")
        "texture_multisampled_2d" -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyTextureMultisampled2D scalar, rest3)
                _ -> Left (errorAt rest "texture_multisampled_2d element must be a scalar")
        "texture_depth_2d" ->
              Right (TyTextureDepth2D, rest)
        "texture_depth_2d_array" ->
              Right (TyTextureDepth2DArray, rest)
        "texture_depth_cube" ->
              Right (TyTextureDepthCube, rest)
        "texture_depth_cube_array" ->
              Right (TyTextureDepthCubeArray, rest)
        "texture_depth_multisampled_2d" ->
              Right (TyTextureDepthMultisampled2D, rest)
        "texture_storage_1d" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture1D fmt access, rest5)
        "texture_storage_2d" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture2D fmt access, rest5)
        "texture_storage_2d_array" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture2DArray fmt access, rest5)
        "texture_storage_3d" -> do
              rest1 <- expectSymbol "<" rest
              (fmt, rest2) <- parseStorageFormat rest1
              rest3 <- expectSymbol "," rest2
              (access, rest4) <- parseStorageAccess rest3
              rest5 <- expectSymbol ">" rest4
              Right (TyStorageTexture3D fmt access, rest5)
        _ | Just (cols, rows) <- parseMatrixName name -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyMatrix cols rows scalar, rest3)
                _ -> Left (errorAt rest "matrix element must be a scalar")
        _ | name `elem` ["vec2", "vec3", "vec4"] -> do
              rest1 <- expectSymbol "<" rest
              (inner, rest2) <- parseType rest1
              rest3 <- expectSymbol ">" rest2
              case inner of
                TyScalar scalar -> Right (TyVector (vecSize name) scalar, rest3)
                _ -> Left (errorAt rest "vector element must be a scalar")
        "array" -> do
              rest1 <- expectSymbol "<" rest
              (elemTy, rest2) <- parseType rest1
              case rest2 of
                (Token (TkSymbol ",") _ : rest3) -> do
                  (lenExpr, rest4) <- parseConstExpr [">"] rest3
                  rest5 <- expectSymbol ">" rest4
                  Right (TyArray elemTy (ArrayLenExpr lenExpr), rest5)
                _ -> do
                  rest3 <- expectSymbol ">" rest2
                  Right (TyArray elemTy ArrayLenRuntime, rest3)
        _ | Just (base, scalar) <- parseTypedScalarSuffix name ->
              case base of
                "vec2" -> Right (TyVector 2 scalar, rest)
                "vec3" -> Right (TyVector 3 scalar, rest)
                "vec4" -> Right (TyVector 4 scalar, rest)
                _ | Just (cols, rows) <- parseMatrixName base ->
                      Right (TyMatrix cols rows scalar, rest)
                _ -> Right (TyStructRef name, rest)
        _ -> Right (TyStructRef name, rest)

parseFullIdent :: [Token] -> Either CompileError (Text, [Token])
parseFullIdent toks = do
  (name, rest) <- parseIdent toks
  go name rest
  where
    go acc rest =
      case rest of
        (Token (TkSymbol "::") _ : more) -> do
          (next, rest1) <- parseIdent more
          go (acc <> "::" <> next) rest1
        _ -> Right (acc, rest)

parseIdent :: [Token] -> Either CompileError (Text, [Token])
parseIdent toks =
  case toks of
    (Token (TkIdent name) _ : rest) -> Right (name, rest)
    _ -> Left (errorAt toks "expected identifier")

expectSymbol :: Text -> [Token] -> Either CompileError [Token]
expectSymbol sym toks =
  case toks of
    (Token (TkSymbol s) _ : rest) | s == sym -> Right rest
    _ -> Left (errorAt toks ("expected symbol '" <> textToString sym <> "'"))

parseScalar :: Text -> Scalar
parseScalar name = case name of
  "i32" -> I32
  "u32" -> U32
  "f16" -> F16
  "f32" -> F32
  "bool" -> Bool
  _ -> I32

parseStorageFormat :: [Token] -> Either CompileError (StorageFormat, [Token])
parseStorageFormat toks =
  case toks of
    (Token (TkIdent name) _ : rest) ->
      case name of
        "rgba8unorm" -> Right (FormatRgba8Unorm, rest)
        "rgba8snorm" -> Right (FormatRgba8Snorm, rest)
        "rgba8uint" -> Right (FormatRgba8Uint, rest)
        "rgba8sint" -> Right (FormatRgba8Sint, rest)
        "rgba16float" -> Right (FormatRgba16Float, rest)
        "rgba16uint" -> Right (FormatRgba16Uint, rest)
        "rgba16sint" -> Right (FormatRgba16Sint, rest)
        "rgba16unorm" -> Right (FormatRgba16Unorm, rest)
        "rgba16snorm" -> Right (FormatRgba16Snorm, rest)
        "rgba32float" -> Right (FormatRgba32Float, rest)
        "rgba32uint" -> Right (FormatRgba32Uint, rest)
        "rgba32sint" -> Right (FormatRgba32Sint, rest)
        "r32float" -> Right (FormatR32Float, rest)
        "r32uint" -> Right (FormatR32Uint, rest)
        "r32sint" -> Right (FormatR32Sint, rest)
        "r16float" -> Right (FormatR16Float, rest)
        "r16uint" -> Right (FormatR16Uint, rest)
        "r16sint" -> Right (FormatR16Sint, rest)
        "r16unorm" -> Right (FormatR16Unorm, rest)
        "r16snorm" -> Right (FormatR16Snorm, rest)
        "r8unorm" -> Right (FormatR8Unorm, rest)
        "r8snorm" -> Right (FormatR8Snorm, rest)
        "r8uint" -> Right (FormatR8Uint, rest)
        "r8sint" -> Right (FormatR8Sint, rest)
        "rg32float" -> Right (FormatRg32Float, rest)
        "rg32uint" -> Right (FormatRg32Uint, rest)
        "rg32sint" -> Right (FormatRg32Sint, rest)
        "rg16float" -> Right (FormatRg16Float, rest)
        "rg16uint" -> Right (FormatRg16Uint, rest)
        "rg16sint" -> Right (FormatRg16Sint, rest)
        "rg16unorm" -> Right (FormatRg16Unorm, rest)
        "rg16snorm" -> Right (FormatRg16Snorm, rest)
        "rg8unorm" -> Right (FormatRg8Unorm, rest)
        "rg8snorm" -> Right (FormatRg8Snorm, rest)
        "rg8uint" -> Right (FormatRg8Uint, rest)
        "rg8sint" -> Right (FormatRg8Sint, rest)
        "rgb10a2unorm" -> Right (FormatRgb10a2Unorm, rest)
        "rgb10a2uint" -> Right (FormatRgb10a2Uint, rest)
        "rg11b10float" -> Right (FormatRg11b10Float, rest)
        _ -> Left (errorAt toks "unknown storage texture format")
    _ -> Left (errorAt toks "expected storage texture format")

parseStorageAccess :: [Token] -> Either CompileError (StorageAccess, [Token])
parseStorageAccess toks =
  case toks of
    (Token (TkIdent name) _ : rest) ->
      case name of
        "read" -> Right (StorageRead, rest)
        "write" -> Right (StorageWrite, rest)
        "read_write" -> Right (StorageReadWrite, rest)
        _ -> Left (errorAt toks "unknown storage texture access")
    _ -> Left (errorAt toks "expected storage texture access")

parsePtrAccessMaybe :: [Token] -> Either CompileError (Maybe StorageAccess, [Token])
parsePtrAccessMaybe toks =
  case toks of
    (Token (TkSymbol ",") _ : Token (TkIdent access) _ : rest) ->
      case access of
        "read" -> Right (Just StorageRead, rest)
        "write" -> Right (Just StorageWrite, rest)
        "read_write" -> Right (Just StorageReadWrite, rest)
        _ -> Left (errorAt toks "unknown pointer access")
    _ -> Right (Nothing, toks)
