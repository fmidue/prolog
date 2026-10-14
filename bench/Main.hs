{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Language.Prolog (Term (..), VariableName (..), consult, resolve)
import Language.Prolog.Quote (ts)

import Control.DeepSeq (NFData (rnf), deepseq)
import System.Environment (getArgs)

instance NFData Term where
  rnf (Struct a ts') = rnf a `seq` rnf ts'
  rnf (Var v) = rnf v
  rnf (Cut n) = rnf n
instance NFData VariableName where
  rnf (VariableName i s) = rnf i `seq` rnf s
  rnf (Wildcard _) = ()

main :: IO ()
main = do
  args <- getArgs
  let n = (case args of [] -> 6; (x : _) -> read x) :: Int
  Right p <- consult "bench/queens.pl"
  putStrLn "Starting benchmark..."
  case resolve p [ts|queens($n,Qs)|] of
    Left err -> putStrLn err
    Right qs ->
      putStrLn $ qs `deepseq` "Number of solutions: " ++ show (length qs)
