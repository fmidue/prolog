{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Database (
  createDB,
  hasPredicate,
  getClauses,
  asserta,
  assertz,
  abolish,
  Signature (..),
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

import Data.Data (Data)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Syntax (Atom, Clause (Clause, predicateArgs, predicateName), Term (..))

data Signature = Signature Atom Int
  deriving (Data, Eq, Generic, Ord)

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
      (\clause -> Map.insertWith (++) (Signature (predicateName clause) $ length $ predicateArgs clause) [clause])
      (Map.fromList [(Signature name 0, []) | name <- emptyPredicates])
      clauses

getClauses :: Term -> Database -> [Clause]
getClauses term (DB index) = fromMaybe [] $ (`Map.lookup` index) =<< termSignature term

asserta :: Term -> Database -> Database
asserta (Struct name args) (DB index) =
  DB $ Map.insertWith (++) (Signature name $ length args) [Clause name args []] index
asserta _ db = db

assertz :: Term -> Database -> Database
assertz (Struct name args) (DB index) =
  DB $ Map.insertWith (flip (++)) (Signature name $ length args) [Clause name args []] index
assertz _ db = db

abolish :: (Atom, [Term]) -> Database -> Database
abolish (factName, factArgs) (DB index) = DB $ Map.adjust deleteFact (Signature factName $ length factArgs) index
  where
    deleteFact (Clause name args [] : cs) | factName == name && args == factArgs = cs
    deleteFact (f : cs) = f : deleteFact cs
    deleteFact [] = []
