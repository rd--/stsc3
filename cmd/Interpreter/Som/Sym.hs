-- | Symbol type.  Used as key for dictionary lookups.
module Interpreter.Som.Sym where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

type Symbol = St.Identifier

symbol :: String -> Symbol
symbol = id

fromSymbol :: Symbol -> String
fromSymbol = id
