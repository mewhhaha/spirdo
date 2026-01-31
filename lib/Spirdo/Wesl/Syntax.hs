-- | AST definitions for WESL.
module Spirdo.Wesl.Syntax
  ( SrcPos(..)
  , Token(..)
  , TokKind(..)
  , IntSuffix(..)
  , FloatSuffix(..)
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
  , WorkgroupSize(..)
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
import Spirdo.Wesl.Types (Attr, BindingKind, ConstExpr, DiagnosticSeverity, StructDecl, Type)

data SrcPos = SrcPos
  { spLine :: !Int
  , spCol :: !Int
  } deriving (Eq, Show)

data Token = Token
  { tkKind :: !TokKind
  , tkPos :: !SrcPos
  } deriving (Eq, Show)

data IntSuffix = IntSuffixI | IntSuffixU
  deriving (Eq, Show)

data FloatSuffix = FloatSuffixF | FloatSuffixH
  deriving (Eq, Show)

data TokKind
  = TkIdent !Text
  | TkInt !Integer !(Maybe IntSuffix)
  | TkFloat !Float !(Maybe FloatSuffix)
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
  , odType :: !(Maybe Type)
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
  , modEntries :: ![EntryPoint]
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
  , cdType :: !(Maybe Type)
  , cdExpr :: !Expr
  } deriving (Eq, Show)

data Stage = StageCompute | StageFragment | StageVertex
  deriving (Eq, Show)

data WorkgroupSize
  = WorkgroupSizeExpr ![ConstExpr]
  | WorkgroupSizeValue !(Word32, Word32, Word32)
  deriving (Eq, Show)

data Param = Param
  { paramPos :: !SrcPos
  , paramName :: !Text
  , paramType :: !Type
  , paramAttrs :: ![Attr]
  } deriving (Eq, Show)

data EntryPoint = EntryPoint
  { epName :: !Text
  , epStage :: !Stage
  , epWorkgroupSize :: !(Maybe WorkgroupSize)
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
  = SLet !SrcPos !Text !(Maybe Type) !Expr
  | SVar !SrcPos !Text !(Maybe Type) !(Maybe Expr)
  | SAssign !SrcPos !LValue !Expr
  | SAssignOp !SrcPos !LValue !BinOp !Expr
  | SInc !SrcPos !LValue
  | SDec !SrcPos !LValue
  | SExpr !SrcPos !Expr
  | SIf !SrcPos !Expr ![Stmt] !(Maybe [Stmt])
  | SWhile !SrcPos !Expr ![Stmt]
  | SLoop !SrcPos ![Stmt] !(Maybe [Stmt])
  | SFor !SrcPos !(Maybe Stmt) !(Maybe Expr) !(Maybe Stmt) ![Stmt]
  | SSwitch !SrcPos !Expr ![SwitchCase] !(Maybe [Stmt])
  | SBreak !SrcPos
  | SBreakIf !SrcPos !Expr
  | SContinue !SrcPos
  | SDiscard !SrcPos
  | SFallthrough !SrcPos
  | SReturn !SrcPos !(Maybe Expr)
  deriving (Eq, Show)

data SwitchCase = SwitchCase
  { scSelectors :: ![Expr]
  , scBody :: ![Stmt]
  } deriving (Eq, Show)

data LValue
  = LVVar !SrcPos !Text
  | LVField !SrcPos !LValue !Text
  | LVIndex !SrcPos !LValue !Expr
  | LVDeref !SrcPos !Expr
  deriving (Eq, Show)

data Expr
  = EVar !SrcPos !Text
  | EInt !SrcPos !Integer
  | EFloat !SrcPos !Float
  | EBool !SrcPos !Bool
  | EBinary !SrcPos !BinOp !Expr !Expr
  | EUnary !SrcPos !UnaryOp !Expr
  | ECall !SrcPos !Text ![Expr]
  | EBitcast !SrcPos !Type !Expr
  | EField !SrcPos !Expr !Text
  | EIndex !SrcPos !Expr !Expr
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
