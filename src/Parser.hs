module Parser
   ( consult, consultString, parseQuery
   , program, whitespace, clause, terms, term, bottom, vname
   ) where

import Text.Parsec
import Text.Parsec.Expr hiding (Assoc(..))
import qualified Text.Parsec.Expr as Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>),(<*>),(<$),(<*))

import Syntax

consult = fmap consultString . readFile

consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"

parseQuery = parse (whitespace >> terms <* eof) "(query)"

program = many (clause <* reservedOp "." <* whitespace)


clause = do t <- struct <* whitespace
            dcg t <|> normal t
   where
      normal t = do
            ts <- option [] $ do reservedOp ":-"
                                 terms
            return (Clause t ts)

      dcg t = do
            reservedOp "-->" <* whitespace
            ts <- terms
            return (translate (t,ts))

      translate ((Struct a ts), rhs) =
         let lhs' = Struct a (arguments ts (head vars) (last vars))
             vars = map (var.("d_"++).(a++).show) [0..length rhs] -- We explicitly choose otherwise invalid variable names
             rhs' = zipWith3 translate' rhs vars (tail vars)
         in Clause lhs' rhs'

      translate' t s s0 | isList t   = Struct "=" [ s, foldr_pl cons s0 t ]     -- Terminal
      translate' t@(Struct "{}" ts) s s0 = foldr and (Struct "=" [ s, s0 ]) ts  -- Braced terms
      translate' (Struct a ts)  s s0 = Struct a (arguments ts s s0)             -- Non-Terminal

      and x y = Struct "," [x,y]



isList (Struct "." [_,_]) = True
isList (Struct "[]" [])   = True
isList _                  = False



terms = splitAtConjunction <$> term

splitAtConjunction :: Term -> [Term]
splitAtConjunction (Struct "," [g,t]) = g : splitAtConjunction  t
splitAtConjunction t = [t]

termWithoutConjunction = term' False
term = term' True

term' ignoreConjunction =
  buildExpressionParser
    (reverse $ map (map toParser) $ hierarchy ignoreConjunction)
    (bottom <* whitespace)

bottom = variable
      <|> struct
      <|> list
      <|> number
      <|> stringLiteral
      <|> cut <$ char '!'
      <|> Struct "{}" <$> braces terms
      <|> parens term
      <|> operatorLiteral

toParser (PrefixOp name)      = Prefix (reservedOp name >> return (\t -> Struct name [t]))
toParser (InfixOp assoc name) = Infix  (reservedOp name >> return (\t1 t2 -> Struct name [t1, t2]))
                                       (case assoc of AssocLeft  -> Parsec.AssocLeft
                                                      AssocRight -> Parsec.AssocRight)


charWs c = char c <* whitespace

variable = (Var newWildcard <$ (lookAhead (char '_') >> identifier))
  <|> (Var <$> vname)
  <?> "variable"

vname = lookAhead upper >> (VariableName 0 <$> identifier)

functor = (lookAhead lower >> identifier)
   <|> operator
   <|> between (char '\'') (char '\'') (many (noneOf "'"))
   <?> "functor"

struct = do
  f <- functor
  ts <- option [] $ parens $ commaSep1 termWithoutConjunction
  return (Struct f ts)

operatorLiteral = Struct <$> operator <*> pure []

list = brackets $ do
  hds <- option [] $ commaSep1 termWithoutConjunction
  tl <- option nil (charWs '|' >> term)
  return $ foldr cons tl hds

number = do
  i <- integer
  return $ Struct (show i) []

-- Prolog syntax definition
langProlog :: P.LanguageDef ()
langProlog = P.LanguageDef
  { P.commentStart = "/*"
  , P.commentEnd = "*/"
  , P.commentLine = "%"
  , P.nestedComments = True
  , P.identStart = letter <|> char '_'
  , P.identLetter = alphaNum <|> char '_'
  , P.opStart = oneOf (map head operatorNames)
  , P.opLetter = oneOf "#$&@*+/<=>\\^~"--sodiv"
  , P.reservedNames = []
  , P.reservedOpNames = [".", ":-", "|", "-->"]
  , P.caseSensitive = True
  }

operatorNames = [ ";", ",", "<", "=..", "=:=", "=\\=", "=<", "=", ">=", ">", "\\=", "is", "*", "+", "-", "\\", "mod", "div", "\\+" ]

-- lexer
lexer = P.makeTokenParser langProlog

reservedOp = P.reservedOp lexer
operator = P.operator lexer
whitespace = P.whiteSpace lexer
natural = P.natural lexer
integer = P.integer lexer
identifier = P.identifier lexer
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
commaSep1 = P.commaSep1 lexer

charLiteral = P.charLiteral lexer
stringLiteral = foldr (cons . representChar) nil <$> P.stringLiteral lexer
representChar c = Struct (show (fromEnum c)) [] -- This is the classical Prolog representation of chars as code points.
--representChar c = Struct [c] [] -- This is the more natural representation as one-character atoms.
--representChar c = Struct "char" [Struct (show (fromEnum c)) []] -- This is a representation as tagged code points.
--toChar :: Term -> Maybe Char
--toChar (Struct "char" [Struct (toEnum . read->c) []]) = Just c
--toChar _                                              = Nothing
