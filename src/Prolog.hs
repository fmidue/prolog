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
   , resolve, resolve_, resolveN, resolveN_, resolveP
   , (+++)
   , consult, consultString, parseQuery
   , program, whitespace, clause, terms, term, bottom, vname
   , runQuery, TraceMode(..), runQuery', runQueryN, runQueryN'
   )
where

import Syntax
import Parser
import Unifier
import Interpreter

import Control.Monad.IO.Class
import Control.Monad.Trans.Writer

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
          Tracing -> case runWriterT $ resolve p q of
            Left e -> printError e
            Right (u,t) -> putStrLn t >> pure u
          NoTracing -> case resolve p q of
            Left e -> printError e
            Right u -> pure u
        Left err -> do
          putStrLn "error parsing query:"
          printError $ show err
    Left err -> do
      putStrLn "error parsing file:"
      printError $ show err

runQueryN :: Int -> Maybe FilePath -> String -> IO [Unifier]
runQueryN = runQueryN' NoTracing

runQueryN' :: TraceMode -> Int -> Maybe FilePath -> String -> IO [Unifier]
runQueryN' t n mFile qstr = do
  r <- case mFile of
    Just file -> consult file
    Nothing -> return $ Right []
  case r of
    Right p -> do
      case parseQuery qstr of
        Right q -> case t of
          Tracing -> case runWriterT $ resolveN n p q of
            Left e -> printError e
            Right (u,t) -> putStrLn t >> pure u
          NoTracing -> case resolveN n p q of
            Left e -> printError e
            Right u -> pure u
        Left err -> do
          putStrLn "error parsing query:"
          printError $ show err
    Left err -> do
      putStrLn "error parsing file:"
      printError $ show err

printError :: Monoid a => String -> IO a
printError err = putStrLn err >> mempty
