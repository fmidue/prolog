{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
#if !MIN_VERSION_base(4,18,0)
{-# LANGUAGE DerivingStrategies #-}
#endif
{-# LANGUAGE RecordWildCards #-}

module Syntax (
  Term (..),
  var,
  cut,
  Clause (..),
  rhs,
  VariableName (..),
  Atom,
  Goal,
  Program,
  newWildcard,
  cons,
  nil,
  foldrPl,
  arguments, -- FIXME Should not be exposed
  hierarchy,
  Operator (..),
  Assoc (..),
  prettifyVariableName,
  prettifyTerm,
  prettifyClause,
)
where

import Data.Char (digitToInt)
import Data.Generics (Data (..))
import Data.List (intercalate, uncons, unsnoc)
#if !MIN_VERSION_base(4,18,0)
import Data.Typeable (Typeable)
#endif
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Syntax (Lift (..))

data Term
  = Struct Atom [Term]
  | Var VariableName
  | Cut Int
  deriving (Data, Eq, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

var :: String -> Term
var = Var . VariableName 0

cut :: Term
cut = Cut 0

data Clause
  = Clause {predicateName :: Atom, predicateArgs :: [Term], rhs_ :: [Goal]}
  | ClauseFn {predicateName :: Atom, predicateArgs :: [Term], fn :: [Term] -> [Goal]}
  deriving (Data, Generic)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

instance Show Clause where
  show c =
    name
      ++ " {predicateName = "
      ++ show (predicateName c)
      ++ ", predicateArgs = "
      ++ show (predicateArgs c)
      ++ ", "
      ++ lastArg
      ++ "}"
    where
      (name, lastArg) = case c of
        Clause {} -> ("Clause", "rhs_ = " ++ show (rhs_ c))
        ClauseFn {} -> ("ClauseFn", "fn = <function>")

rhs :: Clause -> [Term] -> [Goal]
rhs (Clause _ _ rhs') = const rhs'
rhs (ClauseFn _ _ fn') = fn'

data VariableName
  = VariableName Int String
  | Wildcard (Maybe Int)
  deriving (Data, Eq, Generic, Ord, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

newWildcard :: VariableName
newWildcard = Wildcard Nothing

type Atom = String
type Goal = Term
type Program = [Clause]

instance Ord Term where
  (<=) = wildcards <=! variables <=! atoms <=! compoundTerms <=! error "incomparable"

infixr 4 <=!
(<=!) :: Ord a => (t -> Maybe a) -> (t -> t -> Bool) -> t -> t -> Bool
(q <=! _) (q -> Just l) (q -> Just r) = l <= r
(q <=! _) (q -> Just _) _ = True
(q <=! _) _ (q -> Just _) = False
(_ <=! c) x y = c x y

wildcards :: Term -> Maybe (Maybe Int)
wildcards (Var (Wildcard i)) = Just i
wildcards _ = Nothing

variables :: Term -> Maybe VariableName
variables (Var v@(VariableName _ _)) = Just v
variables _ = Nothing

atoms :: Term -> Maybe [Atom]
atoms (Struct a []) = Just [a]
atoms _ = Nothing

compoundTerms :: Term -> Maybe (Int, Atom, [Term])
compoundTerms (Struct a ts) = Just (length ts, a, ts)
compoundTerms _ = Nothing

prettifyTerm :: Term -> String
prettifyTerm = prettyPrint False 0

prettyPrint :: Bool -> Int -> Term -> [Char]
prettyPrint f n (Struct (flip lookup infixOperatorTable -> Just (p, InfixOp assoc name)) [l, r]) =
  parensIf addParens $ prettyPrint f' n_l l ++ spaced name ++ prettyPrint f' n_r r
  where
    addParens = f && name == "," || n >= p
    f' = not addParens && f
    (n_l, n_r) = case assoc of
      AssocLeft -> (p - 1, p)
      AssocRight -> (p, p - 1)
prettyPrint f n (Struct (flip lookup prefixOperatorTable -> Just (p, PrefixOp name)) [r]) =
  parensIf addParens $ name ++ prettyPrint f' p {- Non-associative -} r
  where
    addParens = n >= p
    f' = not addParens && f
prettyPrint _ _ t@(Struct "." [_, _]) =
  let (ts, rest) = g [] t
  in -- case guard (isNil rest) >> sequence (map toChar ts) of
     --   Just str -> prettyPrint str
     --   Nothing  ->
     "["
       ++ intercalate "," (map (prettyPrint True 0) ts)
       ++ (if isNil rest then "" else "|" ++ prettyPrint True 0 rest)
       ++ "]"
  where
    g ts (Struct "." [h, t']) = g (h : ts) t'
    g ts t' = (reverse ts, t')
    isNil (Struct "[]" []) = True
    isNil _ = False
prettyPrint _ _ (Struct a []) = a
prettyPrint _ _ (Struct a ts) = a ++ "(" ++ intercalate ", " (map (prettyPrint True 0) ts) ++ ")"
prettyPrint _ _ (Var v) = prettifyVariableName v
-- prettyPrint _ _ Wildcard        = "_"
prettyPrint _ _ (Cut _) = "!"

-- prettyPrint _ _ ((==cut)->True) = "!"
-- prettyPrint _ _ (Cut n)         = "!^" ++ show n

spaced :: [Char] -> [Char]
spaced s =
  let
    condPrefix = maybe True ((`notElem` ",;") . fst) $ uncons s
    condPostfix = maybe False ((/= ';') . snd) $ unsnoc s
  in
    spaceIf condPrefix ++ s ++ spaceIf condPostfix

spaceIf :: Bool -> String
spaceIf True = " "
spaceIf False = ""

parensIf :: Bool -> String -> String
parensIf True s = "(" ++ s ++ ")"
parensIf False s = s

-- Split by arity so that an operator name used as both prefix and infix
-- (e.g. '-') matches the right rule: prefix '-' has precedence 10, infix '-' 5.
infixOperatorTable :: [(Atom, (Int, Operator))]
infixOperatorTable = concat $ zipWith opInfix [1 ..] (hierarchy True)

prefixOperatorTable :: [(Atom, (Int, Operator))]
prefixOperatorTable = concat $ zipWith opPrefix [1 ..] (hierarchy True)

opInfix :: Int -> [Operator] -> [(Atom, (Int, Operator))]
opInfix p ops = [(name, (p, op)) | op@(InfixOp _ name) <- ops]

opPrefix :: Int -> [Operator] -> [(Atom, (Int, Operator))]
opPrefix p ops = [(name, (p, op)) | op@(PrefixOp name) <- ops]

prettifyVariableName :: VariableName -> String
prettifyVariableName (VariableName 0 v) = v
prettifyVariableName (VariableName i v) = v ++ subscriptDigits i
prettifyVariableName (Wildcard (Just i)) = "_" ++ show i
prettifyVariableName (Wildcard Nothing) = "_"

prettifyClause :: Clause -> String
prettifyClause (Clause name args []) = prettifyTerm (Struct name args) ++ "."
prettifyClause (Clause name args rhs') =
  prettifyTerm (Struct name args)
    ++ " :- "
    ++ prettyPrint False 0 (foldr1 conjunction (concatMap flattenConjunction rhs'))
    ++ "."
prettifyClause (ClauseFn name args _) = prettifyTerm (Struct name args) ++ " :- " ++ "<Haskell function>" ++ "."

foldrPl :: (Term -> a -> a) -> a -> Term -> a
foldrPl f k (Struct "." [h, t]) = f h (foldrPl f k t)
foldrPl _ k (Struct "[]" []) = k
foldrPl _ _ Struct {} = error "foldrPl: not a prolog list"
foldrPl _ _ Var {} = error "foldrPl: not a prolog list"
foldrPl _ _ Cut {} = error "foldrPl: not a prolog list"

cons :: Term -> Term -> Term
cons t1 t2 = Struct "." [t1, t2]

nil :: Term
nil = Struct "[]" []

conjunction :: Term -> Term -> Term
conjunction l r = Struct "," [l, r]

flattenConjunction :: Term -> [Term]
flattenConjunction (Struct "," [l, r]) = flattenConjunction l ++ flattenConjunction r
flattenConjunction t = [t]

data Operator
  = PrefixOp String
  | InfixOp Assoc String
data Assoc
  = AssocLeft
  | AssocRight

-- operators with higher position in this list appear higher in
-- the parsed expression tree. This is because the list is reversed
-- before passed to the parser. FIXME: clean this up/move reverse here?
hierarchy :: Bool -> [[Operator]]
hierarchy ignoreConjunction =
  -- [ [ InfixOp NonAssoc "-->", InfixOp NonAssoc ":-" ]
  [ [infixR ";"]
  , [infixR "," | ignoreConjunction]
  , [prefix "\\+"]
  , map infixL ["<", "=..", "=:=", "=\\=", "=<", "=", ">=", ">", "\\=", "is", "==", "@<", "@=<", "@>=", "@>"]
  , map infixL ["+", "-", "\\"]
  , [infixL "*"]
  , [infixL "mod"]
  , [infixL "div"]
  , [infixL "^", infixL "**"]
  , [prefix "-"]
  , [prefix "$"] -- used for quasi quotation
  ]
  where
    prefix = PrefixOp
    infixL = InfixOp AssocLeft
    infixR = InfixOp AssocRight

-- infix 6 \\
-- x \\ y = Struct "\\" [x,y]

arguments :: [a] -> a -> a -> [a]
arguments ts xs ds = ts ++ [xs, ds]

-- arguments ts xs ds = [ xs \\ ds ] ++ ts

subscriptDigits :: Int -> String
subscriptDigits 0 = "₀"
subscriptDigits 1 = "₁"
subscriptDigits 2 = "₂"
subscriptDigits 3 = "₃"
subscriptDigits 4 = "₄"
subscriptDigits 5 = "₅"
subscriptDigits 6 = "₆"
subscriptDigits 7 = "₇"
subscriptDigits 8 = "₈"
subscriptDigits 9 = "₉"
subscriptDigits i
  | i < 0 = error "negative number in identifier subscript"
  | otherwise = concatMap (subscriptDigits . digitToInt) $ show i

$(deriveLiftMany [''Term, ''VariableName, ''Clause])

instance Lift ([Term] -> [Goal]) where
  lift _ = error "Clauses using Haskell functions can't be lifted."
  liftTyped = error "Clauses using Haskell functions can't be lifted."
