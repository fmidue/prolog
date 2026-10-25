module PrettyPrintingSpec where

import Control.Monad (forM_)
import Language.Prolog (Clause (Clause), Term (Struct), consultString)
import Test.HUnit (assertFailure)
import Test.Hspec (Spec, describe, it, shouldBe)

dis :: Term -> Term -> Term
dis l r = Struct ";" [l, r]

con :: Term -> Term -> Term
con l r = Struct "," [l, r]

a, b, c, d :: Term
a = Struct "a" []
b = Struct "b" []
c = Struct "c" []
d = Struct "d" []

preservations :: [(String, String)]
preservations =
  [ ("p :- (a;b),c,d.", "p :- (a;b), c, d.")
  , ("p :- a;b,c,d.", "p :- a;b, c, d.")
  , ("p :- a;(b,c,d).", "p :- a;b, c, d.")
  , ("p :- (a;(b,(c,d))).", "p :- a;b, c, d.")
  , ("p :- a;b, (c, d).", "p :- a;b, c, d.")
  , ("p :- (a, b); c.", "p :- a, b;c.")
  , ("p :- a ; b ; c.", "p :- a;b;c.")
  , ("p :- a, ! ; b ; c.", "p :- a, !;b;c.")
  , ("p :- (a ; b) ; c.", "p :- (a;b);c.")
  , ("p :- a, b, c.", "p :- a, b, c.")
  , ("p :- \\+ a ; b.", "p :- \\+a;b.")
  , ("p :- not(a) ; b.", "p :- not(a);b.")
  , ("p :- \\+ (a, b).", "p :- \\+(a, b).")
  , ("p :- not(a, b).", "p :- not(a, b).")
  , ("p(X) :- X = [a, b, c].", "p(X) :- X = [a,b,c].")
  , ("p(X) :- X = [a, b|c].", "p(X) :- X = [a,b|c].")
  , ("p(X,Y,Z) :- X is Y + Z.", "p(X, Y, Z) :- X is Y + Z.")
  , ("p(X,Y,Z) :- X is Y * Y + Z.", "p(X, Y, Z) :- X is Y * Y + Z.")
  , ("p(X,Y,Z) :- X is Y * (Y + Z).", "p(X, Y, Z) :- X is Y * (Y + Z).")
  , ("p(X) :- X =.. [Y, Z].", "p(X) :- X =.. [Y,Z].")
  , ("p :- q(a, g(b, c)); h.", "p :- q(a, g(b, c));h.")
  , ("p(X) :- X = Y, g(Y).", "p(X) :- X = Y, g(Y).")
  , ("p(X) :- (X = Y, Y = Z), g(Z).", "p(X) :- X = Y, Y = Z, g(Z).")
  , ("p(X, Y) :- X is -Y.", "p(X, Y) :- X is -Y.")
  , ("p(X, Y, Z) :- X is -Y - Z.", "p(X, Y, Z) :- X is -Y - Z.")
  , ("p(X, Y, Z) :- X is -(Y - Z).", "p(X, Y, Z) :- X is -(Y - Z).")
  ]

expectations :: [(Clause, String)]
expectations =
  [ -- a ; (b, (c, d))
    (Clause "p" [] [dis a (con b (con c d))], "p :- a;b, c, d.")
  , -- a ; (b, c)
    (Clause "p" [] [dis a (con b c)], "p :- a;b, c.")
  , -- (a, (b, c)) ; d
    (Clause "p" [] [dis (con a (con b c)) d], "p :- a, b, c;d.")
  ]

spec :: Spec
spec = describe "pretty printing" $ do
  describe "it should preserve semantics but remove redundant parentheses" $ do
    forM_ preservations $ \(input, expected) -> do
      it input $ case consultString input of
        Left err -> assertFailure $ show err
        Right [(n, args, goals)] -> show (Clause n args goals) `shouldBe` expected
        _ -> assertFailure "only one clause should be parsed"

  describe "it should correctly print the given value" $ do
    forM_ expectations $ \(input, expected) -> do
      it (show input) $
        show input `shouldBe` expected
