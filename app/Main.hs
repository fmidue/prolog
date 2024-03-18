module Main where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Except (catchError)
import Control.Exception (displayException)

import Data.List (intercalate)

import System.Console.Haskeline
import System.Environment (getArgs)

import Paths_prolog (version)
import Data.Version (showVersion)

import Language.Prolog

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runMain $ Just file
    [] -> runMain Nothing
    _ -> putStrLn "usage: prolog-interpreter [<file>]"

runMain :: Maybe FilePath -> IO ()
runMain file = do
  putStrLn $ unwords ["prolog-interpreter", "version", showVersion version, "(Ctrl-D to exit)"]
  case file of
    Nothing -> pure ()
    Just fname -> do
      putStrLn $ unwords ["using definitions form", fname]
      putStrLn "changes to the file do not require reloading"
  runInputT settings (loop NoTracing)
  where
    settings = Settings {
      complete = completeFilename,
      historyFile = Just ".prolog-interpreter.history",
      autoAddHistory = True
      }
    loop :: TraceMode -> InputT IO ()
    loop traceMode = do
      query <- getInputLine "?- "
      -- parsing can't handle the . so it is removed (if present)
      let query' = (\q -> if last q == '.' then init q else q) <$> query
      case query' of
        Nothing -> pure () -- EOF / Ctrl-D
        Just "trace" -> loop Tracing
        Just "notrace" -> loop NoTracing
        Just q -> do
          us <- lift $
            catchError
              (runQuery' traceMode file q)
              -- simple exeption handeling (mainly to avoid crashes)
              (\e -> putStrLn (displayException e) >> pure [])
          outputStr $ printUnifiers us
          loop traceMode

printUnifiers :: [Unifier] -> String
printUnifiers [] = "false.\n"
printUnifiers us = unlines [ printUnifier u | u <- us ]
  where
    printUnifier :: Unifier -> String
    printUnifier [] = "true."
    printUnifier u = intercalate ",\n" (map printSubst u) ++ "."

    printSubst :: (VariableName, Term) -> String
    printSubst (x,t) = unwords [show x, "=", show t]
