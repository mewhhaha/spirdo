-- | AST definitions for WESL.
module Spirdo.Wesl.Syntax
  ( SrcPos(..)
  , Token(..)
  , TokKind(..)
  , ImportRelative(..)
  , ImportItem(..)
  , ImportDecl(..)
  , Directive(..)
  , AliasDecl(..)
  , OverrideDecl(..)
  , ConstAssert(..)
  , ModuleAst(..)
  , BindingDecl(..)
  , GlobalVarDecl(..)
  , ConstDecl(..)
  , Stage(..)
  , Param(..)
  , EntryPoint(..)
  , FunctionDecl(..)
  , Stmt(..)
  , SwitchCase(..)
  , LValue(..)
  , Expr(..)
  , BinOp(..)
  , UnaryOp(..)
  ) where

import Data.Text (Text)
import Data.Word (Word32)
import Spirdo.Wesl.Types (Attr, BindingKind, DiagnosticSeverity, StructDecl, Type)

data SrcPos = SrcPos
  { spLine :: !Int
  , spCol :: !Int
  } deriving (Eq, Show)

data Token = Token
  { tkKind :: !TokKind
  , tkPos :: !SrcPos
  } deriving (Eq, Show)

data TokKind
  = TkIdent !Text
  | TkInt !Integer
  | TkFloat !Float
  | TkString !Text
  | TkSymbol !Text
  deriving (Eq, Show)

data ImportRelative
  = ImportPackage
  | ImportSuper Int
  deriving (Eq, Show)

data ImportItem = ImportItem
  { iiPath :: ![Text]
  , iiAlias :: !(Maybe Text)
  } deriving (Eq, Show)

data ImportDecl = ImportDecl
  { idRelative :: !(Maybe ImportRelative)
  , idItems :: ![ImportItem]
  } deriving (Eq, Show)

data Directive
  = DirEnable !Text
  | DirDiagnostic !DiagnosticSeverity !Text
  deriving (Eq, Show)

data AliasDecl = AliasDecl
  { adName :: !Text
  , adType :: !Type
  } deriving (Eq, Show)

data OverrideDecl = OverrideDecl
  { odName :: !Text
  , odId :: !(Maybe Word32)
  , odType :: !Type
  , odExpr :: !(Maybe Expr)
  } deriving (Eq, Show)

data ConstAssert = ConstAssert
  { caPos :: !SrcPos
  , caExpr :: !Expr
  } deriving (Eq, Show)

data ModuleAst = ModuleAst
  { modDirectives :: ![Directive]
  , modImports :: ![ImportDecl]
  , modAliases :: ![AliasDecl]
  , modStructs :: ![StructDecl]
  , modBindings :: ![BindingDecl]
  , modGlobals :: ![GlobalVarDecl]
  , modConsts :: ![ConstDecl]
  , modOverrides :: ![OverrideDecl]
  , modConstAsserts :: ![ConstAssert]
  , modFunctions :: ![FunctionDecl]
  , modEntry :: !(Maybe EntryPoint)
  } deriving (Eq, Show)

data BindingDecl = BindingDecl
  { bdName :: !Text
  , bdKind :: !BindingKind
  , bdGroup :: !Word32
  , bdBinding :: !Word32
  , bdType :: !Type
  } deriving (Eq, Show)

data GlobalVarDecl = GlobalVarDecl
  { gvName :: !Text
  , gvSpace :: !Text
  , gvType :: !Type
  , gvInit :: !(Maybe Expr)
  } deriving (Eq, Show)

data ConstDecl = ConstDecl
  { cdName :: !Text
  , cdExpr :: !Expr
  } deriving (Eq, Show)

data Stage = StageCompute | StageFragment | StageVertex
  deriving (Eq, Show)

data Param = Param
  { paramName :: !Text
  , paramType :: !Type
  , paramAttrs :: ![Attr]
  } deriving (Eq, Show)

data EntryPoint = EntryPoint
  { epName :: !Text
  , epStage :: !Stage
  , epWorkgroupSize :: !(Maybe (Word32, Word32, Word32))
  , epParams :: ![Param]
  , epReturnType :: !(Maybe Type)
  , epReturnLocation :: !(Maybe Word32)
  , epReturnBuiltin :: !(Maybe Text)
  , epBody :: ![Stmt]
  } deriving (Eq, Show)

data FunctionDecl = FunctionDecl
  { fnName :: !Text
  , fnParams :: ![Param]
  , fnReturnType :: !(Maybe Type)
  , fnBody :: ![Stmt]
  } deriving (Eq, Show)

data Stmt
  = SLet !Text !Expr
  | SVar !Text !Expr
  | SAssign !LValue !Expr
  | SAssignOp !LValue !BinOp !Expr
  | SInc !LValue
  | SDec !LValue
  | SExpr !Expr
  | SIf !Expr ![Stmt] !(Maybe [Stmt])
  | SWhile !Expr ![Stmt]
  | SLoop ![Stmt] !(Maybe [Stmt])
  | SFor !(Maybe Stmt) !(Maybe Expr) !(Maybe Stmt) ![Stmt]
  | SSwitch !Expr ![SwitchCase] !(Maybe [Stmt])
  | SBreak
  | SBreakIf !Expr
  | SContinue
  | SDiscard
  | SFallthrough
  | SReturn !(Maybe Expr)
  deriving (Eq, Show)

data SwitchCase = SwitchCase
  { scSelectors :: ![Expr]
  , scBody :: ![Stmt]
  } deriving (Eq, Show)

data LValue
  = LVVar !Text
  | LVField !LValue !Text
  | LVIndex !LValue !Expr
  | LVDeref !Expr
  deriving (Eq, Show)

data Expr
  = EVar !Text
  | EInt !Integer
  | EFloat !Float
  | EBool !Bool
  | EBinary !BinOp !Expr !Expr
  | EUnary !UnaryOp !Expr
  | ECall !Text ![Expr]
  | EBitcast !Type !Expr
  | EField !Expr !Text
  | EIndex !Expr !Expr
  deriving (Eq, Show)

data BinOp
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  | OpEq
  | OpNe
  | OpLt
  | OpLe
  | OpGt
  | OpGe
  | OpAnd
  | OpOr
  | OpBitAnd
  | OpBitOr
  | OpBitXor
  | OpShl
  | OpShr
  deriving (Eq, Show)

data UnaryOp = OpNeg | OpNot | OpAddr | OpDeref
  deriving (Eq, Show)
