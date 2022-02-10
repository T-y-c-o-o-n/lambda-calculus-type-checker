module Base where

type Var = String

type TypeVar = String

infixl 4 :@

data Type
  = T TypeVar
  | Type :=> Type
  deriving (Ord, Eq, Show)

data Term
  = V Var
  | Term :@ Term
  | Term :@: Type
  | L Var Type Term
  deriving (Ord, Eq, Show)
