module Language.Potion.Syntax where

type Name = String

data Expression
  = EApp Expression [Expression]
  | ECase [(Pattern, Expression)]
  | EFun [Name] Expression
  | ELet [(Pattern, Expression)] Expression
  | EL Literal
  | EN Name
  deriving (Eq, Show)

data Pattern
  = PApp Pattern [Pattern] -- [1, 2] | ...test | Ctor(test)
  | PAt Name Pattern       -- name@[a, ...]
  | PLit Literal           -- 3 | "test" | 'c'
  | PName Name             -- Ctor | {} | ...
  | PWild                  -- _
  deriving (Eq, Show)

data Declaration
  = DFun Name [Name] Expression
--   | DSpec Name Type
--   -- | DData
--   -- | DInterface
--   -- | DInstance
  deriving (Eq, Show)

-- data BindGroup = BG Name [Pattern] Expression

-- data Expression
--   = Lit Literal
--   | List [Expression]
--   | Map [(Expression, Expression)]
--   | Var Name
--   | TName Name
--   | Op Name
--   | Func [Name] Expression
--   | Tuple [Expression]
--   | App Expression [Expression]
--   | Noop
--   deriving (Eq, Show)

data Literal
  = LB Bool
  | LI Integer
  | LF Double
  | LC Char
  | LS String
  deriving (Eq, Show)

-- type Declaration = (Name, Expression)

data Program
  = Program [Import] [(Access, Declaration)]
  deriving (Eq, Show)

data Access
  = Private
  | Public
  deriving (Eq, Show)

data Import
  = Import Name Name -- import repo.module
  deriving (Eq, Show)
