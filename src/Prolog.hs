module Prolog
   ( Term(..), var, cut
   , Clause(..), rhs
   , VariableName(..), Atom, Unifier, Substitution, Program, Goal
   , unify, unify_with_occurs_check
   , apply
   , MonadTrace(..)
   , withTrace
   , MonadGraphGen(..)
   , runNoGraphT
   , resolve, resolve_
   , (+++)
   , consult, consultString, parseQuery
   , program, whitespace, clause, terms, term, bottom, vname
   , runQuery, TraceMode(..), runQuery'
   )
where

import Syntax
import Parser
import Unifier
import Interpreter

import Control.Monad.IO.Class

runQuery :: Maybe FilePath -> String -> IO [Unifier]
runQuery = runQuery' NoTracing

data TraceMode = Tracing | NoTracing

runQuery' :: TraceMode -> Maybe FilePath -> String -> IO [Unifier]
runQuery' t mFile qstr = do
  r <- case mFile of
    Just file -> consult file
    Nothing -> return $ Right []
  case r of
    Right p -> do
      case parseQuery qstr of
        Right q -> case t of
          Tracing -> withTrace $ resolve p q
          NoTracing -> resolve p q
        Left err -> do
          putStrLn "error parsing query:"
          print err
          pure []
    Left err -> do
      putStrLn "error parsing file:"
      print err
      pure []
