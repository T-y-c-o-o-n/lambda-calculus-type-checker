module Base where

type Var = String
type TypeVar = String
type Context = [(Var, Type)]
type TypeInference = (Context, Term, Type)

infixl 4 :@
infixr 4 :=>

data Type
  = T TypeVar
  | Type :=> Type
  deriving (Ord, Eq, Show)

data Term
  = V Var
  | Term :@ Term
  | L Var Type Term
  deriving (Ord, Eq, Show)
