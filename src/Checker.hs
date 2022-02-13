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
  case M.lookup x ctx of
    Nothing -> do
      t <- generateNewType
      modify $ first $ M.insert x t
      return t
    Just t -> return t
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
typeEquations (e :@. t) = do
  tE <- typeEquations e
  t' <- generateNewType
  modify $ second $ first ((tE, t :=> t') :)
  return t'
typeEquations (LL a e) = do
  tE <- typeEquations e
  return $ ForAll a tE

subst :: TypeVar -> Type -> Type -> Type
subst a t other@(T b) = if a == b then t else other
subst a t (t1 :=> t2) = subst a t t1 :=> subst a t t2
subst a t other@(ForAll b t') = if a == b then other else ForAll b $ subst a t t'

containsTypeVar :: TypeVar -> Type -> Bool
containsTypeVar a (T y) = a == y
containsTypeVar a (t1 :=> t2) = containsTypeVar a t1 || containsTypeVar a t2
containsTypeVar a (ForAll b t) = a /= b && containsTypeVar a t

unification :: S.Set TypeVar -> (Context, [(Type, Type)]) -> Either String Context
unification _ (ctx, []) = Right ctx
unification unknown (ctx, (t1@(T a), t2@(T b)) : eqs)
  | a == b = unification unknown (ctx, eqs)
  | a `S.member` unknown = unification unknown (ctx, map (bimap (subst a t2) (subst a t2)) eqs)
  | b `S.member` unknown = unification unknown (ctx, (t2, t1) : eqs)
  | otherwise = Left $ "Couldn't match expected type " ++ b ++ " with actual type " ++ a
unification unknown (ctx, (T a, t2) : eqs)
  | containsTypeVar a t2 = Left $ "Occurs check: " ++ a ++ " = " ++ show t2
  | a `S.notMember` unknown = Left $ "Couldn't match expected type " ++ show t2 ++ " with actual type " ++ a
  | otherwise = unification unknown (M.insert a t2 ctx, map (bimap (subst a t2) (subst a t2)) eqs)
unification unknown (ctx, (t1, t2@(T _)) : eqs) =
  if t1 == t2
    then unification unknown (ctx, eqs)
    else unification unknown (ctx, (t2, t1) : eqs)
unification unknown (ctx, (t1 :=> t2, t1' :=> t2') : eqs) = unification unknown (ctx, (t1, t1') : (t2, t2') : eqs)
unification unknown (ctx, (ForAll a t, ForAll b t') : eqs) = unification unknown (ctx, (T a, T b) : (t, t') : eqs)
unification _ (_, (t, t') : _) = Left $ "Cannot match " ++ show t ++ " with " ++ show t'

findAllTypeNames :: Type -> S.Set TypeVar
findAllTypeNames (T a) = S.singleton a
findAllTypeNames (t1 :=> t2) = S.union (findAllTypeNames t1) (findAllTypeNames t2)
findAllTypeNames (ForAll _ t) = findAllTypeNames t

findAllTypeNamesFromContext :: Context -> S.Set TypeVar
findAllTypeNamesFromContext ctx = foldl S.union S.empty $ map findAllTypeNames $ M.elems ctx

check :: TypeInference -> Either (Type, String) Context
check (ctx, term, usersType) =
  let (actualType, (ctx', (eqs, (_, (unknown, _))))) = runState (typeEquations term) (ctx, ([], (findAllTypeNamesFromContext ctx `S.union` findAllTypeNames usersType, (S.empty, 0))))
   in case unification unknown (ctx', (usersType, actualType) : eqs) of
     Left e -> Left (actualType, e)
     Right ctx -> Right ctx
