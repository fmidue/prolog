module Database
   ( createDB
   , hasPredicate
   , getClauses
   , asserta
   , assertz
   , abolish
   , Signature(), termSignature
   , Database
   )
where

import Data.Map (Map)
import qualified Data.Map.Strict as Map

import Syntax
import Data.Maybe (fromMaybe)


data Signature = Signature Atom Int deriving (Ord, Eq)
instance Show Signature where
   show (Signature name arity) = name ++ "/" ++ show arity

termSignature :: Term -> Maybe Signature
termSignature (Struct name ts) = Just $ Signature name (length ts)
termSignature (Var _) = Nothing
termSignature (Cut _) = Nothing


newtype Database = DB (Map Signature [Clause])

hasPredicate sig (DB index) = Map.member sig index

createDB clauses emptyPredicates = DB $
   foldr (\clause -> maybe id (\sig -> Map.insertWith (++) sig [clause]) (termSignature (lhs clause)))
         (Map.fromList [ (Signature name 0, []) | name <- emptyPredicates ])
         clauses

getClauses term (DB index) = fromMaybe [] $ (`Map.lookup` index) =<< termSignature term


asserta fact (DB index) = DB $ maybe index (\sig -> Map.insertWith (++)        sig [Clause fact []] index) (termSignature fact)
assertz fact (DB index) = DB $ maybe index (\sig -> Map.insertWith (flip (++)) sig [Clause fact []] index) (termSignature fact)
abolish fact (DB index) = DB $ maybe index (\sig -> Map.adjust deleteFact sig index) (termSignature fact)
   where deleteFact (Clause t []:cs) | t == fact = cs
         deleteFact (_          :cs)             = cs
         deleteFact []                           = []
