{
module Language.Smalltalk.Spl.Lexer where

import Language.Smalltalk.Spl.Token
}

%wrapper "basic"

$digit = 0-9 -- 3.5.1
$lower = a-z -- 3.5.1 uppercaseAlphabetic
$upper = A-Z -- 3.5.1 lowercaseAlphabetic
$underscore = _ -- 3.5.1 nonCaseLetter
$letter = [a-z A-Z _] -- 3.5.1 letter
$letter_or_digit = [a-z A-Z _ 0-9]
$binary_char = [\! \@ \% \& \* \- \+ \= \| \< \> \? \/ \~ \^ \#] -- !@%&*-+=|<>?/~^#
$graphic = $printable # $white
$commentary = [a-z A-Z 0-9 \( \) \[ \] \' \" \. \; \= \, \: \_ \  \t \n \! \@ \% \& \* \- \+ \= \| \< \> \? \/ \~ \^ \#]

@identifier = $letter $letter_or_digit*
@decimal = $digit+
@integer = \-? @decimal
@float = \-? @decimal \. @decimal
@commentary = ($printable | [\n])

tokens :-

  <0> $white+                                ;
  <0> "/*" $commentary+ "*/"          ;

  <0> "("                                    { \_ -> LeftParen }
  <0> ")"                                    { \_ -> RightParen }
  <0> ","                                    { \_ -> Comma }
  <0> "."                                    { \_ -> Dot }
  <0> ".."                                   { \_ -> DotDot }
  <0> ":"                                    { \_ -> Colon }
  <0> "::"                                   { \_ -> ColonColon }
  <0> ":="                                   { \_ -> ColonEquals }
  <0> ";"                                    { \_ -> SemiColon }
  <0> "="                                    { \_ -> Equals }
  <0> "["                                    { \_ -> LeftBracket }
  <0> "]"                                    { \_ -> RightBracket }
  <0> "{"                                    { \_ -> LeftBrace }
  <0> "|"                                    { \_ -> VerticalBar }
  <0> "}"                                    { \_ -> RightBrace }

  <0> "false"                                { \_ -> FalseIdentifier }
  <0> "let"                                  { \_ -> Let }
  <0> "nil"                                  { \_ -> NilIdentifier }
  <0> "true"                                 { \_ -> TrueIdentifier }

  <0> @identifier                            { \s -> Identifier s }
  <0> @identifier ":/" @integer              { \s -> ArityQualifiedIdentifier s }
  <0> $binary_char+                          { \s -> BinarySelector s }
  <0> @float                                 { \s -> Float (read s) }
  <0> @integer                               { \s -> Integer (read s) }
  <0> \" ($printable # \")* \"               { \s -> DoubleQuotedString (removeOuter 1 s) }
  <0> \' ($printable # \')* \'               { \s -> SingleQuotedString (removeOuter 1 s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - k) (drop k x)
}
