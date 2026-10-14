{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Monad (forM, void)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import System.Console.ANSI
import System.Directory
import System.Environment (getArgs)
import System.FilePath

import Test.HUnit hiding (path)
import Text.Parsec

import Data.Functor.Identity (Identity)
import Language.Prolog (Program, Term, VariableName, bottom, consult, consultString, parseQuery, resolve, term, vname, whitespace)

main :: IO ()
main = do
  args <- getArgs
  let specRoot = case args of [] -> "specs"; [path] -> path; _ -> error "too many arguments"
  files <- filter ((".spec" ==) . takeExtension) . map (specRoot </>) <$> getDirectoryContents specRoot
  tests <- forM files $ \fname -> do
    let fixture = replaceExtension fname ".pl"
    hasFixture <- doesFileExist fixture
    p <-
      if hasFixture
        then consult fixture
        else return $ Right []
    text <- readFile fname
    case parse specFile fname text of
      Left err -> return (dropExtension fname ~: (assertFailure (show err) :: IO ()))
      Right tests -> return (dropExtension fname ~: tests p)
  colorizeResult =<< runTestTT (test (parserMessageTests ++ tests))

colorizeResult :: Counts -> IO ()
colorizeResult result = do
  let color = case result of
        Counts _ _ 0 0 -> Green
        _ -> Red
  cursorUpLine 1
  setSGR [SetColor Background Dull color, SetColor Foreground Dull Black]
  putStr (showCounts result)
  setSGR [Reset]
  putStrLn ""

parserMessageTests :: [Test]
parserMessageTests =
  [ "file parser reports spaced functor arguments" ~: assertParseErrorContains (consultString "isX(a (x)).")
  , "file parser reports multiply spaced predicate arguments" ~: assertParseErrorContains (consultString "isX  (x).")
  , "query parser reports spaced predicate arguments" ~: assertParseErrorContains (parseQuery "isX (x)")
  , "query parser reports multiply spaced predicate arguments" ~: assertParseErrorContains (parseQuery "isX  (x)")
  ]

assertParseErrorContains :: Either ParseError a -> Assertion
assertParseErrorContains result =
  case result of
    Left err ->
      assertBool (show err) ("whitespace between predicate and arguments" `isInfixOf` show err)
    Right _ ->
      assertFailure "expected parse failure"

specFile :: Parsec String () (Either ParseError Program -> [Test])
specFile =
  (pure . ("parsing should fail" ~:) . (@?= True) . isLeft) <$ try (string "syntax error")
    <|> (\specs -> pure $ either (pure . ("" ~:) . assertFailure @(IO ()) . show) (\p -> map ($ p) specs))
    =<< (testSpec `sepBy` newline <* eof)

testSpec :: Parsec String () (Program -> Test)
testSpec = do
  q <- string "?-" >> whitespace >> term <* string "." <* newline
  us <- unifiers <* optional (whitespace >> string ";" >> newline >> string "false") <* string "." <* (void newline <|> eof)
  return $ \p -> "?- " ++ show q ++ "." ~: either (\e -> putStrLn e >> pure []) pure (resolve p [q]) >>= (@?= us)

unifiers :: ParsecT String () Identity [[(VariableName, Term)]]
unifiers =
  (unifier `sepBy1` try (string ";" <* newline >> notFollowedBy (string "false.")))
    <|> (string "false" >> return [])

unifier :: ParsecT String () Identity [(VariableName, Term)]
unifier =
  (substitution `sepBy1` (string "," <* newline))
    <|> (string "true" >> return [])

substitution :: ParsecT String () Identity (VariableName, Term)
substitution = (,) <$> vname <* string "=" <* whitespace <*> bottom
