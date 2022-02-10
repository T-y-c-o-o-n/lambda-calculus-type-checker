module Checker where

import Base
import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
import qualified Data.Map as M
import qualified Data.Set as S

generateNewType :: State (Context, ([(Type, Type)], (S.Set Type, Int))) Type
generateNewType = do
  (ctx, (eqs, (types, i))) <- get
  let newType = T $ "v" ++ show i
   in if newType `S.member` types
        then do
          modify $ second $ second $ second (+ 1)
          generateNewType
        else do
          put (ctx, (eqs, (newType `S.insert` types, i + 1)))
          return newType

typeEquations :: Term -> State (Context, ([(Type, Type)], (S.Set Type, Int))) Type
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
subst x t other@(T y) = if x == y then t else other
subst x t (t1 :=> t2) = subst x t t1 :=> subst x t t2

containsX :: TypeVar -> Type -> Bool
containsX x (T y) = x == y
containsX x (t1 :=> t2) = containsX x t1 || containsX x t2

unification :: (Context, [(Type, Type)]) -> Either String Context
unification (ctx, []) = Right ctx
unification (ctx, (t1, t2@(T _)) : eqs) =
  if t1 == t2
    then unification (ctx, eqs)
    else unification (ctx, (t2, t1) : eqs)
unification (ctx, (T x, t2) : eqs) =
  if containsX x t2
    then Left $ "occurs check: " ++ x ++ " = " ++ show t2
    else unification (M.insert x t2 ctx, map (bimap (subst x t2) (subst x t2)) eqs)
unification (ctx, (t1 :=> t2, t1' :=> t2') : eqs) = unification (ctx, (t1, t1') : (t2, t2') : eqs)

check :: TypeInference -> Either String Context
check (ctx, term, expectedType) = 
  let (actualType, (ctx', (eqs, _))) = runState (typeEquations term) (ctx, ([], (S.fromAscList $ M.elems ctx, 0))) in
    unification (ctx', (expectedType, actualType) : eqs)
