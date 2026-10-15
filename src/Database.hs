module Database (
  createDB,
  hasPredicate,
  getClauses,
  asserta,
  assertz,
  abolish,
  Signature (),
  termSignature,
  Database,
)
where

import Data.Map (Map)
import qualified Data.Map.Strict as Map (
  adjust,
  fromList,
  insertWith,
  lookup,
  member,
 )

import Data.Maybe (fromMaybe)
import Syntax (Atom, Clause (Clause), Term (..), lhs)

data Signature = Signature Atom Int deriving (Eq, Ord)
instance Show Signature where
  show (Signature name arity) = name ++ "/" ++ show arity

termSignature :: Term -> Maybe Signature
termSignature (Struct name ts) = Just $ Signature name (length ts)
termSignature (Var _) = Nothing
termSignature (Cut _) = Nothing

newtype Database = DB (Map Signature [Clause])

hasPredicate :: Signature -> Database -> Bool
hasPredicate sig (DB index) = Map.member sig index

createDB :: Foldable t => t Clause -> [Atom] -> Database
createDB clauses emptyPredicates =
  DB $
    foldr
      (\clause -> maybe id (\sig -> Map.insertWith (++) sig [clause]) (termSignature (lhs clause)))
      (Map.fromList [(Signature name 0, []) | name <- emptyPredicates])
      clauses

getClauses :: Term -> Database -> [Clause]
getClauses term (DB index) = fromMaybe [] $ (`Map.lookup` index) =<< termSignature term

asserta :: Term -> Database -> Database
asserta fact@(Struct name args) (DB index) =
  DB $
    maybe
      index
      (\sig -> Map.insertWith (++) sig [Clause name args []] index)
      (termSignature fact)
asserta _ _ = error ""

assertz :: Term -> Database -> Database
assertz fact@(Struct name args) (DB index) =
  DB $
    maybe
      index
      (\sig -> Map.insertWith (flip (++)) sig [Clause name args []] index)
      (termSignature fact)
assertz _ _ = error ""

abolish :: Term -> Database -> Database
abolish fact (DB index) = DB $ maybe index (\sig -> Map.adjust deleteFact sig index) (termSignature fact)
  where
    deleteFact (c@(Clause _ _ []) : cs) | lhs c == fact = cs
    deleteFact (_ : cs) = cs
    deleteFact [] = []
