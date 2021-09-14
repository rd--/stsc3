{- | Symbol type.
     Used as key for dictionary lookups.
     Aligned with Ansi AST Identifier type.
-}
module Interpreter.Som.Sym where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

type Symbol = St.Identifier

toSymbol :: String -> Symbol
toSymbol = id

fromSymbol :: Symbol -> String
fromSymbol = id
