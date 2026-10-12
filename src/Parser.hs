module Parser
   ( consult, consultString, parseQuery
   , program, whitespace, clause, terms, term, bottom, vname
   ) where

import Text.Parsec
import Text.Parsec.Expr hiding (Assoc(..))
import qualified Text.Parsec.Expr as Parsec
import qualified Text.Parsec.Token as P

import Syntax
import Data.Functor.Identity (Identity)

consult :: FilePath -> IO (Either ParseError Program)
consult = fmap consultString . readFile

consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"

parseQuery :: String -> Either ParseError [Term]
parseQuery = parse (whitespace >> terms <* eof) "(query)"

program :: ParsecT String () Identity [Clause]
program = many (clause <* reservedOp "." <* whitespace)


clause :: ParsecT String () Identity Clause
clause = do t <- struct <* whitespace
            dcg t <|> normal t
   where
      normal t = do
            ts <- option [] $ do
              -- custom "expecting" label to avoid showing ":-" twice
              reservedOp ":-" <|> reservedOp ":−" <?> show ":-"
              terms
            return (Clause t ts)

      dcg t = do
            reservedOp "-->" <* whitespace
            ts <- terms
            case translate (t,ts) of
              Just x -> return x
              Nothing -> parserFail "unexpected syntax in dcg-rule"

      translate (Struct a ts, rhs) =
         let lhs' = Struct a (arguments ts (head vars) (last vars))
             vars = map (var.("d_"++).(a++).show) [0..length rhs] -- We explicitly choose otherwise invalid variable names
             rhs' = sequenceA $ zipWith3 translate' rhs vars (tail vars)
         in Clause lhs' <$> rhs'
      translate (Var _, _) = Nothing
      translate (Cut _, _) = Nothing

      translate' t s s0 | isList t   = Just $ Struct "=" [ s, foldr_pl cons s0 t ]     -- Terminal
      translate' t@(Struct "{}" ts) s s0 = Just $ foldr and (Struct "=" [ s, s0 ]) ts  -- Braced terms
      translate' (Struct a ts)  s s0 = Just $ Struct a (arguments ts s s0)             -- Non-Terminal
      translate' (Var _) _ _ = Nothing
      translate' (Cut _) _ _ = Nothing

      and x y = Struct "," [x,y]



isList :: Term -> Bool
isList (Struct "." [_,_]) = True
isList (Struct "[]" [])   = True
isList _                  = False



terms :: ParsecT String () Identity [Term]
terms = splitAtConjunction <$> term

splitAtConjunction :: Term -> [Term]
splitAtConjunction (Struct "," [g,t]) = g : splitAtConjunction  t
splitAtConjunction t = [t]

termWithoutConjunction :: ParsecT String () Identity Term
termWithoutConjunction = term' False

term :: ParsecT String () Identity Term
term = term' True

term' :: Bool -> ParsecT String () Identity Term
term' ignoreConjunction =
  buildExpressionParser
    (reverse $ map (map toParser) $ hierarchy ignoreConjunction)
    (bottom <* whitespace)

bottom :: ParsecT String () Identity Term
bottom = variable
      <|> struct
      <|> list
      <|> number
      <|> stringLiteral
      <|> cut <$ char '!'
      <|> Struct "{}" <$> braces terms
      <|> parens term
      <|> operatorLiteral

toParser :: Syntax.Operator -> Parsec.Operator String () Identity Term
toParser (PrefixOp name)      = Prefix (reservedOp name >> return (\t -> Struct name [t]))
toParser (InfixOp assoc name) = Infix  (reservedOp name >> return (\t1 t2 -> Struct name [t1, t2]))
                                       (case assoc of AssocLeft  -> Parsec.AssocLeft
                                                      AssocRight -> Parsec.AssocRight)


charWs :: Char -> ParsecT String () Identity Char
charWs c = char c <* whitespace

variable :: ParsecT String () Identity Term
variable = (Var newWildcard <$ char '_')
  <|> (Var <$> vname)
  <?> "variable"

vname :: ParsecT String () Identity VariableName
vname = lookAhead upper >> (VariableName 0 <$> identifier)

functor :: ParsecT String () Identity String
functor = (lookAhead lower >> identifier)
   <|> operator
   <|> between (char '\'') (char '\'') (many (noneOf "'"))
   <?> "functor"

struct :: ParsecT String () Identity Term
struct = do
  f <- functor
  ts <- option [] $ parens $ commaSep1 termWithoutConjunction
  return (Struct f ts)

operatorLiteral :: ParsecT String () Identity Term
operatorLiteral = Struct <$> operator <*> pure []

list :: ParsecT String () Identity Term
list = brackets $ do
  hds <- option [] $ commaSep1 termWithoutConjunction
  tl <- option nil (charWs '|' >> termWithoutConjunction)
  return $ foldr cons tl hds

number :: ParsecT String () Identity Term
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
  , P.identStart = letter
  , P.identLetter = letter <|> digit <|> char '_'
  , P.opStart = oneOf (map head operatorNames)
  , P.opLetter = oneOf "#$&@*+/<=>\\^~"--sodiv"
  , P.reservedNames = []
  , P.reservedOpNames = [".", ":-", ":−", "|", "-->"]
  , P.caseSensitive = True
  }

operatorNames :: [String]
operatorNames = [ ";", ",", "<", "=..", "=:=", "=\\=", "=<", "=", ">=", ">", "\\=", "is", "^", "**", "*", "+", "-", "\\", "mod", "div", "\\+" ]

-- lexer
lexer :: P.GenTokenParser String () Identity
lexer = P.makeTokenParser langProlog

reservedOp :: String -> ParsecT String () Identity ()
reservedOp = P.reservedOp lexer

operator :: ParsecT String () Identity String
operator = P.operator lexer

whitespace :: ParsecT String () Identity ()
whitespace = P.whiteSpace lexer

natural :: ParsecT String () Identity Integer
natural = P.natural lexer

integer :: ParsecT String () Identity Integer
integer = P.integer lexer

identifier :: ParsecT String () Identity String
identifier = P.identifier lexer

parens :: ParsecT String () Identity a -> ParsecT String () Identity a
parens = P.parens lexer

brackets :: ParsecT String () Identity a -> ParsecT String () Identity a
brackets = P.brackets lexer

braces :: ParsecT String () Identity a -> ParsecT String () Identity a
braces = P.braces lexer

commaSep1 :: ParsecT String () Identity a -> ParsecT String () Identity [a]
commaSep1 = P.commaSep1 lexer

charLiteral :: ParsecT String () Identity Char
charLiteral = P.charLiteral lexer

stringLiteral :: ParsecT String () Identity Term
stringLiteral = foldr (cons . representChar) nil <$> P.stringLiteral lexer

representChar :: Enum a => a -> Term
representChar c = Struct (show (fromEnum c)) [] -- This is the classical Prolog representation of chars as code points.
--representChar c = Struct [c] [] -- This is the more natural representation as one-character atoms.
--representChar c = Struct "char" [Struct (show (fromEnum c)) []] -- This is a representation as tagged code points.
--toChar :: Term -> Maybe Char
--toChar (Struct "char" [Struct (toEnum . read->c) []]) = Just c
--toChar _                                              = Nothing
