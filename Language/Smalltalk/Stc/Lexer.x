{
module Language.Smalltalk.Stc.Lexer where

import qualified Data.Char
import qualified Data.List
import qualified Numeric

import Language.Smalltalk.Stc.Token
}

%wrapper "basic"

$digit = 0-9 -- 3.5.1
$lower = a-z -- 3.5.1 uppercaseAlphabetic
$upper = A-Z -- 3.5.1 lowercaseAlphabetic
$underscore = _ -- 3.5.1 nonCaseLetter
$letter = [a-z A-Z _] -- 3.5.1 letter
$letter_or_digit = [a-z A-Z _ 0-9]
$letter_or_digit_or_colon = [a-z A-Z _ 0-9 \:]
$binary_character = [\!\@\%\&\*\-\+\=\|\<\>\?\/] -- !@%&*-+=|<>?/
$graphic = $printable # $white

@identifier = $letter $letter_or_digit*
@radix_integer = \-? $digit+ r $letter_or_digit+
@decimal_integer = \-? $digit+
@decimal_float = \-? $digit+ \. $digit+

tokens :-

  $white+                                ;
  "/*" ($printable # \*)* "*/"           ;
  "//" $printable+                       ;

  "#["                                   { \_ -> HashLeftBracket }
  "("                                    { \_ -> LeftParen }
  ")"                                    { \_ -> RightParen }
  "*"                                    { \_ -> Asterisk }
  "+"                                    { \_ -> Plus }
  ","                                    { \_ -> Comma }
  "."                                    { \_ -> Dot }
  ".."                                   { \_ -> DotDot }
  ":"                                    { \_ -> Colon }
  "::"                                   { \_ -> ColonColon }
  ";"                                    { \_ -> SemiColon }
  "="                                    { \_ -> Equals }
  "["                                    { \_ -> LeftBracket }
  "]"                                    { \_ -> RightBracket }
  "{"                                    { \_ -> LeftBrace }
  "|"                                    { \_ -> VerticalBar }
  "}"                                    { \_ -> RightBrace }

  "arg"                                  { \_ -> Arg }
  "classvar"                             { \_ -> ClassVar }
  "false"                                { \_ -> FalseIdentifier }
  "nil"                                  { \_ -> NilIdentifier }
  "self"                                 { \_ -> SelfIdentifier }
  "true"                                 { \_ -> TrueIdentifier }
  "var"                                  { \_ -> Var }

  ":="                                   { \_ -> AssignmentOperator }
  "^"                                    { \_ -> ReturnOperator }

  "$" [$graphic \ ]                      { \s -> QuotedCharacter (s !! 1) }
  $binary_character+                     { \s -> BinarySelector s }
  @identifier                            { \s -> Identifier s }
  @identifier ":"                        { \s -> Keyword (init s) }
  $letter $letter_or_digit_or_colon* ":" { \s -> KeywordSelector s }
  @decimal_float                         { \s -> Float (read s) }
  @radix_integer                         { \s -> Integer (parseRadixInteger s) }
  @decimal_integer                       { \s -> Integer (read s) }
  \" ($printable # \")* \"               { \s -> DoubleQuotedString (removeOuter 1 s) }
  \' ($printable # \')* \'               { \s -> SingleQuotedString (removeOuter 1 s) }
  \\ $letter $letter_or_digit*           { \s -> SingleQuotedString (tail s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - 1) (drop k x)

parseRadixInteger :: String -> Integer
parseRadixInteger s =
  case Data.List.break (== 'r') s of
    (lhs, _ : rhs) ->
      let signedBase = read lhs
          base = abs signedBase
          sign = signum signedBase
          charToInt x = if Data.Char.isDigit x then Data.Char.digitToInt x else fromEnum (Data.Char.toLower x) - 87
          charToIntegral = fromIntegral . charToInt
          isValid x = let i = charToIntegral x in i >= 0 && i < base
      in case Numeric.readInt base isValid charToInt rhs of
           [(r,_)] -> r * signum sign
           _ -> error "parseRadixInteger: encoding error"
    _ -> error "parseRadixInteger: syntax error"

}
