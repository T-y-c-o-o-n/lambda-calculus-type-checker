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
  | ForAll TypeVar Type
  deriving (Ord, Eq)

instance Show Type where
  show (T a) = a
  show (t1@(T _) :=> t2) = show t1 ++ " -> " ++ show t2
  show (t1 :=> t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (ForAll a t) = "@" ++ a ++ ". " ++ show t

data Term
  = V Var
  | Term :@ Term
  | L Var Type Term
  | LL TypeVar Term
  | Term :@. Type
  deriving (Ord, Eq)

instance Show Term where
  show (V x) = x
  show (t1 :@ t2@(V _)) = show t1 ++ " " ++ show t2
  show (t1 :@ t2) = show t1 ++ " (" ++ show t2 ++ ")"
  show (L x t term) = "\\" ++ x ++ ": " ++ show t ++ ". " ++ show term
  show (LL a term) = "/\\" ++ a ++ ". " ++ show term
  show (term :@. t) = show term ++ " (" ++ show t ++ ")"
