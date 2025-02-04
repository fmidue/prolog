{-# LANGUAGE DeriveDataTypeable, ViewPatterns, ScopedTypeVariables #-}
module Syntax
   ( Term(..), var, cut
   , Clause(..), rhs
   , VariableName(..), Atom, Goal, Program
   , newWildcard
   , cons, nil, foldr_pl
   , arguments -- FIXME Should not be exposed
   , hierarchy
   , Operator(..), Assoc(..)
   )
where

import Data.Generics (Data(..), Typeable(..))
import Data.List (intercalate)
import Data.Char (isLetter)


data Term = Struct Atom [Term]
          | Var VariableName
          | Cut Int
      deriving (Eq, Data, Typeable)
var = Var . VariableName 0
cut = Cut 0

data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable)
rhs (Clause   _ rhs) = const rhs
rhs (ClauseFn _ fn ) = fn

data VariableName
  = VariableName Int String
  | Wildcard (Maybe Int)
  deriving (Eq, Data, Typeable, Ord)

newWildcard = Wildcard Nothing

type Atom         = String
type Goal         = Term
type Program      = [Clause]

instance Ord Term where
   (<=) = wildcards <=! variables <=! atoms <=! compound_terms <=! error "incomparable"

infixr 4 <=!
(q <=! _) (q->Just l) (q->Just r) = l <= r
(q <=! _) (q->Just _) _ = True
(q <=! _) _ (q->Just _) = False
(_ <=! c) x y = c x y

wildcards (Var (Wildcard i)) = Just i
wildcards _        = Nothing

variables (Var v@(VariableName _ _)) = Just v
variables _       = Nothing

numbers (Struct (reads->[(n :: Integer,"")]) []) = Just n
numbers _                                        = Nothing

atoms (Struct a []) = Just [a]
atoms _             = Nothing

compound_terms (Struct a ts) = Just (length ts, a, ts)
compound_terms _             = Nothing


instance Show Term where
   show = prettyPrint False 0


prettyPrint True _ t@(Struct "," [_,_]) =
   "(" ++ prettyPrint False 0 t ++  ")"

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,InfixOp assoc name)) [l,r]) =
   parensIf (n >= p) $ prettyPrint f n_l l ++ spaced name ++ prettyPrint f n_r r
     where (n_l,n_r) = case assoc of
                           AssocLeft  -> (p-1, p)
                           AssocRight -> (p, p-1)

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,PrefixOp name)) [r]) =
   parensIf (n >= p) $ name ++ prettyPrint f (p {- Non-associative -}) r

prettyPrint _ _ t@(Struct "." [_,_]) =
   let (ts,rest) = g [] t in
      --case guard (isNil rest) >> sequence (map toChar ts) of
      --   Just str -> prettyPrint str
      --   Nothing  ->
            "[" ++ intercalate "," (map (prettyPrint True 0) ts) ++ (if isNil rest then "" else "|" ++ (prettyPrint True 0) rest) ++  "]"
   where g ts (Struct "." [h,t]) = g (h:ts) t
         g ts t = (reverse ts, t)
         isNil (Struct "[]" []) = True
         isNil _                = False

prettyPrint _ _ (Struct a [])   = a
prettyPrint _ _ (Struct a ts)   = a ++ "(" ++ intercalate ", " (map (prettyPrint True 0) ts) ++ ")"
prettyPrint _ _ (Var v)         = show v
-- prettyPrint _ _ Wildcard        = "_"
prettyPrint _ _ (Cut _)         = "!"
--prettyPrint _ _ ((==cut)->True) = "!"
--prettyPrint _ _ (Cut n)         = "!^" ++ show n


spaced s = let h = head s
               l = last s
           in spaceIf (isLetter h) ++ s ++ spaceIf (isLetter l || ',' == l)

spaceIf True  = " "
spaceIf False = ""

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++")"
parensIf False s = s


operatorTable :: [(String, (Int,Operator))]
operatorTable = concat $ zipWith (map . g) [1..] $ hierarchy False
 where g p op@(InfixOp _ name) = (name,(p,op))
       g p op@(PrefixOp name)  = (name,(p,op))

instance Show VariableName where
   show (VariableName 0 v) = v
   show (VariableName i v) = v ++ "#" ++  show i
   show (Wildcard (Just i)) = "_" ++  show i
   show (Wildcard Nothing) = "_"

instance Show Clause where
   show (Clause   lhs [] ) = show lhs ++ "."
   show (Clause   lhs rhs) = show lhs ++ " :- " ++ intercalate ", " (map show rhs) ++ "."
   show (ClauseFn lhs _  ) = show lhs ++ " :- " ++ "<Haskell function>" ++ "."




foldr_pl :: (Term -> a -> a) -> a -> Term -> a
foldr_pl f k (Struct "." [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct "[]" [])   = k
foldr_pl _ _ Struct{}   = error "foldr_pl: not a prolog list"
foldr_pl _ _ Var{}   = error "foldr_pl: not a prolog list"
foldr_pl _ _ Cut{}   = error "foldr_pl: not a prolog list"

cons t1 t2 = Struct "."  [t1,t2]
nil        = Struct "[]" []

data Operator = PrefixOp String
              | InfixOp Assoc String
data Assoc = AssocLeft
           | AssocRight

-- operators with higher position in this list appear higher in
-- the parsed expression tree. This is because the list is reversed
-- before passed to the parser. FIXME: clean this up/move reverse here?
hierarchy :: Bool -> [[Operator]]
hierarchy ignoreConjunction =
   --[ [ InfixOp NonAssoc "-->", InfixOp NonAssoc ":-" ]
   [ [ infixR ";" ]
   , [ infixR "," | ignoreConjunction ]
   , [ prefix "\\+" ]
   , map infixL ["<", "=..", "=:=", "=\\=", "=<", "=", ">=", ">", "\\=", "is", "==", "@<", "@=<", "@>=", "@>"]
   , map infixL ["+", "-", "\\"]
   , [ infixL "*"]
   , [ infixL "mod" ]
   , [ infixL "div" ]
   , [ prefix "-" ]
   , [ prefix "$" ] -- used for quasi quotation
   ]
 where
   prefix = PrefixOp
   infixL = InfixOp AssocLeft
   infixR = InfixOp AssocRight


--infix 6 \\
--x \\ y = Struct "\\" [x,y]

arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts
