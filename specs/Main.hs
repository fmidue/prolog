{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Data.Either (isLeft, either)
import Control.Applicative ((<*),(<*>),(<$>))
import Control.Monad (forM)
import System.Directory
import System.FilePath
import System.Console.ANSI
import System.Environment (getArgs)

import Test.HUnit
import Text.Parsec

import Language.Prolog (vname, term, bottom, whitespace, resolve, consult, Program)


main = do
  args <- getArgs
  let specRoot = case args of [] -> "specs"; [path] -> path
  files <- filter ((".spec"==) . takeExtension) <$> map (specRoot </>) <$> getDirectoryContents specRoot
  tests <- forM files $ \fname -> do
    let fixture = replaceExtension fname ".pl"
    hasFixture <- doesFileExist fixture
    p <- if hasFixture
            then consult fixture
            else return $ Right []
    text <- readFile fname
    case parse specFile fname text of
      Left err -> return (dropExtension fname ~: (assertFailure (show err) :: IO ()))
      Right tests -> return (dropExtension fname ~: tests p)
  colorizeResult =<< runTestTT (test tests)

colorizeResult result = do
  let color = case result of Counts _ _ 0 0 -> Green
                             _              -> Red
  cursorUpLine 1
  setSGR [SetColor Background Dull color, SetColor Foreground Dull Black]
  putStr (showCounts result)
  setSGR [Reset]
  putStrLn ""


specFile :: Parsec String () (Either ParseError Program -> [Test])
specFile = (pure . ("parsing should fail" ~:) . (@?= True) . isLeft) <$ try (string "syntax error")
        <|> (\specs -> pure $ either (pure . ("" ~:) . assertFailure @(IO ()) . show) (\p -> map ($ p) specs)) =<< (testSpec `sepBy` newline <* eof)

testSpec :: Parsec String () (Program -> Test)
testSpec = do
  q  <- string "?-" >> whitespace >> term <* string "." <* newline
  us <- unifiers <* optional (whitespace >> string ";" >> newline >> string "false") <* string "." <* ((newline >> return ()) <|> eof)
  return $ \p -> "?- " ++ show q ++ "." ~: either (\e -> putStrLn e >> pure []) pure (resolve p [q]) >>= (@?= us)

unifiers =  (unifier `sepBy1` (try $ string ";" <* newline >> notFollowedBy (string "false.")))
        <|> (string "false" >> return [])

unifier =  (substitution `sepBy1` (string "," <* newline))
       <|> (string "true" >> return [])

substitution = (,) <$> vname <* string "=" <* whitespace <*> bottom
