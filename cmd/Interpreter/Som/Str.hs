-- | String type.  Requires efficient substring function.
module Interpreter.Som.Str where

import qualified Data.Text as Text {- text -}
import qualified Data.Text.Read as Text.Read {- text -}

import Interpreter.Som.Int

type UnicodeString = Text.Text

toUnicodeString :: String -> UnicodeString
toUnicodeString = Text.pack

fromUnicodeString :: UnicodeString -> String
fromUnicodeString = Text.unpack

stringReadInteger :: UnicodeString -> Maybe LargeInteger
stringReadInteger = either (const Nothing) (Just . fst) . Text.Read.signed Text.Read.decimal

stringReadDouble :: UnicodeString -> Maybe Double
stringReadDouble = either (const Nothing) (Just . fst) . Text.Read.double

stringSubstringFromTo :: UnicodeString -> LargeInteger -> LargeInteger -> UnicodeString
stringSubstringFromTo x i j = Text.drop (fromLargeInteger i - 1) (Text.take (fromLargeInteger j) x)

