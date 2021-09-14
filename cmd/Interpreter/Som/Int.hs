{- | Type for Smalltalk integers.
     The Som testsuite requires >64bit.
-}
module Interpreter.Som.Int where

import Data.Bits {- base -}

import qualified Music.Theory.Math.Convert as Math.Convert {- hmt-base -}

type SmallInteger = Int
type LargeInteger = Integer

fromLargeInteger :: LargeInteger -> SmallInteger
fromLargeInteger = fromIntegral

toLargeInteger :: SmallInteger -> LargeInteger
toLargeInteger = fromIntegral

shiftLeft ::  LargeInteger -> LargeInteger ->  LargeInteger
shiftLeft x by = Data.Bits.shiftL x (fromLargeInteger by)

shiftRight ::  LargeInteger -> LargeInteger ->  LargeInteger
shiftRight x by = Data.Bits.shiftR x (fromLargeInteger by)

as32BitUnsignedValue :: LargeInteger -> LargeInteger
as32BitUnsignedValue = Math.Convert.word32_to_integer . Math.Convert.integer_to_word32

as32BitSignedValue :: LargeInteger -> LargeInteger
as32BitSignedValue = Math.Convert.int32_to_integer . Math.Convert.integer_to_int32
