{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Quote (t, ts, c, pl) where

import Data.Generics (Data, extQ, typeOf)
import Language.Haskell.TH (Exp, Pat, Q, listE, mkName, varE, viewP)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift))
import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

import Parser (clause, program, term, terms, whitespace)
import Syntax (Clause (..), Term (..))

t :: QuasiQuoter
t = prologQuasiQuoter term "term"

ts :: QuasiQuoter
ts = prologQuasiQuoter terms "term list"

c :: QuasiQuoter
c = prologQuasiQuoter clause "clause"

pl :: QuasiQuoter
pl = prologQuasiQuoter program "program"

prologQuasiQuoter :: (Data a, Lift a) => Parser a -> String -> QuasiQuoter
prologQuasiQuoter parser name =
  QuasiQuoter {
    quoteExp = parsePrologExp parser name
    , quotePat = parsePrologPat parser name
    , quoteType = \_ -> fail ("Prolog " ++ name ++ "s can't be Haskell types!")
    , quoteDec = \_ -> fail ("Prolog " ++ name ++ "s can't be Haskell declarations!")
    }

parsePrologExp :: (Data a, Lift a) => Parser a -> String -> String -> Q Exp
parsePrologExp parser name str = do
  case parse (whitespace >> parser <* eof) ("(Prolog " ++ name ++ " expression)") str of
    Right x ->
      const (fail $ "Quasi-quoted expressions of type " ++ show (typeOf x) ++ " are not implemented.")
        `extQ` unquote -- Term
        `extQ` (listE . map unquote) -- [Term]
        `extQ` unquoteClause -- Clause
        `extQ` (listE . map unquoteClause) -- [Clause]
        $ x
    Left e -> fail (show e)
  where
    unquote (Struct "$" [Struct var []]) =
      [e|Struct (show $(varE (mkName var))) []|]
    unquote (Struct "$" _) = fail "Found '$' with non-unquotable arguments"
    unquote (Struct a ts') = [e|Struct a $(listE $ map unquote ts')|]
    unquote t' = lift t'

    unquoteClause (Clause lhs' rhs) =
      [e|Clause $(unquote lhs') $(listE $ map unquote rhs)|]
    unquoteClause (ClauseFn _ _) =
      fail "Clauses using Haskell functions are not quasi-quotable."

parsePrologPat :: (Data a, Lift a) => Parser a -> String -> String -> Q Pat
parsePrologPat parser name str = do
  case parse (whitespace >> parser <* eof) ("(Prolog " ++ name ++ " pattern)") str of
    Right x -> viewP [e|(== $(lift x))|] [p|True|]
    Left err -> error $ show err
