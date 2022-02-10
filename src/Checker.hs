module Checker where

import Base
import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import qualified Data.Map as M
import qualified Data.Set as S

generateNewType :: State (Context, ([(Type, Type)], (S.Set TypeVar, (S.Set TypeVar, Int)))) Type
generateNewType = do
  (ctx, (eqs, (typeVars, (unknown, i)))) <- get
  let name = "a" ++ show i
   in let newType = T name
       in if name `S.member` typeVars
            then do
              modify $ second $ second $ second $ second (+ 1)
              generateNewType
            else do
              put (ctx, (eqs, (name `S.insert` typeVars, (name `S.insert` unknown, i + 1))))
              return newType

typeEquations :: Term -> State (Context, ([(Type, Type)], (S.Set TypeVar, (S.Set TypeVar, Int)))) Type
typeEquations (V x) = do
  (ctx, _) <- get
  maybe generateNewType return (M.lookup x ctx)
typeEquations (e :@ f) = do
  tE <- typeEquations e
  tF <- typeEquations f
  t <- generateNewType
  modify $ second $ first ((tE, tF :=> t) :)
  return t
typeEquations (L x t e) = do
  xValueWas <- gets $ fst . first (M.lookup x)
  modify $ first $ M.insert x t
  tE <- typeEquations e
  case xValueWas of
    Nothing -> modify $ first $ M.delete x
    Just tX -> modify $ first $ M.insert x tX
  return (t :=> tE)

subst :: TypeVar -> Type -> Type -> Type
subst a t other@(T b) = if a == b then t else other
subst a t (t1 :=> t2) = subst a t t1 :=> subst a t t2

containsX :: TypeVar -> Type -> Bool
containsX x (T y) = x == y
containsX x (t1 :=> t2) = containsX x t1 || containsX x t2

unification :: S.Set TypeVar -> (Context, [(Type, Type)]) -> Either String Context
unification unknown (ctx, []) = Right ctx
unification unknown (ctx, (t1@(T a), t2@(T b)) : eqs)
  | a == b = unification unknown (ctx, eqs)
  | a `S.member` unknown = unification unknown (ctx, map (bimap (subst a t2) (subst a t2)) eqs)
  | b `S.member` unknown = unification unknown (ctx, (t2, t1) : eqs)
  | otherwise = Left $ "Couldn't match expected type " ++ b ++ " with actual type " ++ a
unification unknown (ctx, (T a, t2) : eqs)
  | containsX a t2 = Left $ "Occurs check: " ++ a ++ " = " ++ show t2
  | a `S.notMember` unknown = Left $ "Couldn't match expected type " ++ show t2 ++ " with actual type " ++ a
  | otherwise = unification unknown (M.insert a t2 ctx, map (bimap (subst a t2) (subst a t2)) eqs)
unification unknown (ctx, (t1, t2@(T _)) : eqs) =
  if t1 == t2
    then unification unknown (ctx, eqs)
    else unification unknown (ctx, (t2, t1) : eqs)
unification unknown (ctx, (t1 :=> t2, t1' :=> t2') : eqs) = unification unknown (ctx, (t1, t1') : (t2, t2') : eqs)

findAllTypeNames :: Type -> S.Set TypeVar
findAllTypeNames (T a) = S.singleton a
findAllTypeNames (t1 :=> t2) = S.union (findAllTypeNames t1) (findAllTypeNames t2)

findAllTypeNamesFromContext :: Context -> S.Set TypeVar
findAllTypeNamesFromContext ctx = foldl S.union S.empty $ map findAllTypeNames $ M.elems ctx

check :: TypeInference -> Either String Context
check (ctx, term, usersType) =
  let (actualType, (ctx', (eqs, (_, (unknown, _))))) = runState (typeEquations term) (ctx, ([], (findAllTypeNamesFromContext ctx `S.union` findAllTypeNames usersType, (S.empty, 0))))
   in unification unknown (ctx', (usersType, actualType) : eqs)

--f :: a -> b -> c
--f x y = y x

--   in Left ("usersType: " ++ show usersType ++ "; actualType: " ++ show actualType ++ "; ctx: " ++ show ctx ++ "; eqs: " ++ show eqs ++ "; unknown: " ++ show unknown)
-- f : beta |- \x: alpha. f (f (f x) ) : alpha -> alpha
-- |- \f: a0. \x: a1. f x : a0 -> a1 -> a2