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

@identifier = $letter $letter_or_digit*
@decimal = $digit+
@integer = \-? @decimal
@float = \-? @decimal \. @decimal

tokens :-

  $white+                                ;
  "/*" ($printable # \*)* "*/"           ;

  "("                                    { \_ -> LeftParen }
  ")"                                    { \_ -> RightParen }
  ","                                    { \_ -> Comma }
  "."                                    { \_ -> Dot }
  ".."                                   { \_ -> DotDot }
  ":"                                    { \_ -> Colon }
  "::"                                   { \_ -> ColonColon }
  ":="                                   { \_ -> ColonEquals }
  ";"                                    { \_ -> SemiColon }
  "="                                    { \_ -> Equals }
  "["                                    { \_ -> LeftBracket }
  "]"                                    { \_ -> RightBracket }
  "{"                                    { \_ -> LeftBrace }
  "|"                                    { \_ -> VerticalBar }
  "}"                                    { \_ -> RightBrace }

  "false"                                { \_ -> FalseIdentifier }
  "let"                                  { \_ -> Let }
  "nil"                                  { \_ -> NilIdentifier }
  "true"                                 { \_ -> TrueIdentifier }

  @identifier                            { \s -> Identifier s }
  @identifier ":/" @integer              { \s -> ArityQualifiedIdentifier s }
  $binary_char+                          { \s -> BinarySelector s }
  @float                                 { \s -> Float (read s) }
  @integer                               { \s -> Integer (read s) }
  \" ($printable # \")* \"               { \s -> DoubleQuotedString (removeOuter 1 s) }
  \' ($printable # \')* \'               { \s -> SingleQuotedString (removeOuter 1 s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - k) (drop k x)
}
