module Base where

import Data.Map

type Var = String
type TypeVar = String
type Context = Map Var Type
type TypeInference = (Context, Term, Type)

infixl 4 :@
infixr 4 :=>

data Type
  = T TypeVar
  | Type :=> Type
  deriving (Ord, Eq)

instance Show Type where
  show (T a) = a
  show (t1@(T _) :=> t2) = show t1 ++ " -> " ++ show t2
  show (t1 :=> t2) = "(" ++ show t1 ++ ") -> " ++ show t2

data Term
  = V Var
  | Term :@ Term
  | L Var Type Term
  deriving (Ord, Eq)

instance Show Term where
  show (V x) = x
  show (t1 :@ t2) = show t1 ++ " " ++ show t2
  show (L x t term) = "\\" ++ x ++ ": " ++ show t ++ ". " ++ show term
