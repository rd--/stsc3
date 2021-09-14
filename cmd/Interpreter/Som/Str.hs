{- | String type.
     Requires efficient substring function.
     SOM has no Char type.
-}
module Interpreter.Som.Str where

import qualified Data.Text as Text {- text -}
import qualified Data.Text.Read as Text.Read {- text -}

import Interpreter.Som.Int {- stsc3 -}

type UnicodeString = Text.Text

toUnicodeString :: String -> UnicodeString
toUnicodeString = Text.pack

fromUnicodeString :: UnicodeString -> String
fromUnicodeString = Text.unpack

unicodeStringReadInteger :: UnicodeString -> Maybe LargeInteger
unicodeStringReadInteger = either (const Nothing) (Just . fst) . Text.Read.signed Text.Read.decimal

unicodeStringReadDouble :: UnicodeString -> Maybe Double
unicodeStringReadDouble = either (const Nothing) (Just . fst) . Text.Read.double

unicodeStringSubstringFromTo :: UnicodeString -> LargeInteger -> LargeInteger -> UnicodeString
unicodeStringSubstringFromTo x i j = Text.drop (fromLargeInteger i - 1) (Text.take (fromLargeInteger j) x)
