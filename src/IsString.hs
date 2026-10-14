-- Allow specification through string literals by using OverloadedStrings.
--
{-# OPTIONS_GHC -Wno-orphans #-}
module IsString () where

import GHC.Exts (IsString(..))
import Text.Parsec (parse, eof)

import Parser ( vname, term, clause )
import Syntax ( VariableName, Term, Clause )


instance IsString Clause where
   fromString s =
      case parse (clause <* eof) "(Clause literal)" s of
         Left  e -> error (show e)
         Right c -> c
instance IsString Term where
   fromString s =
      case parse (term <* eof) "(Term literal)" s of
         Left  e -> error (show e)
         Right c -> c
instance IsString VariableName where
   fromString s =
      case parse (vname <* eof) "(VariableName literal)" s of
         Left  e -> error (show e)
         Right c -> c
