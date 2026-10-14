module Unifier (
  Unifier,
  Substitution,
  unify,
  unifyWithOccursCheck,
  apply,
  (+++),
  substitute,
)
where

import Control.Arrow (second)
import Data.Function (fix)
import Data.Generics (everything, mkQ)

import Syntax (Term (Struct, Var), VariableName)

type Unifier = [Substitution]
type Substitution = (VariableName, Term)

unify, unifyWithOccursCheck :: MonadFail m => Term -> Term -> m Unifier
unify = fix unify'
unifyWithOccursCheck =
  fix $ \self t1 t2 ->
    if t1 `occursIn` t2 || t2 `occursIn` t1
      then fail "occurs check"
      else unify' self t1 t2
  where
    occursIn t = everything (||) (mkQ False (== t))

unify' :: MonadFail m => (Term -> Term -> m Unifier) -> Term -> Term -> m [(VariableName, Term)]
unify' _ (Var v) t = return [(v, t)]
unify' _ t (Var v) = return [(v, t)]
unify' self (Struct a1 ts1) (Struct a2 ts2)
  | a1 == a2 && same length ts1 ts2 =
      unifyList self (zip ts1 ts2)
unify' _ _ _ = fail "unify'"

same :: Eq b => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

unifyList :: Monad m => (Term -> Term -> m Unifier) -> [(Term, Term)] -> m Unifier
unifyList _ [] = return []
unifyList unifyFn ((x, y) : xys) = do
  u <- unifyFn x y
  u' <- unifyList unifyFn (map (both (apply u)) xys)
  return (u ++ u')

both :: (t -> b) -> (t, t) -> (b, b)
both f (x, y) = (f x, f y)

(+++) :: [Substitution] -> [Substitution] -> Unifier
u1 +++ u2 = simplify $ u1 ++ u2

simplify :: Unifier -> Unifier
simplify u = map (second (apply u)) u

apply :: Unifier -> Term -> Term
apply = flip $ foldl $ flip substitute

substitute :: (VariableName, Term) -> Term -> Term
substitute (v, t) (Var v') | v == v' = t
substitute s (Struct a ts) = Struct a (map (substitute s) ts)
substitute _ t = t
