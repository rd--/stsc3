{
module Language.Smalltalk.Stc.Lexer where

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
$binary_char = [\!\@\%\&\*\-\+\=\|\<\>\?\/] -- !@%&*-+=|<>?/
$graphic = $printable # $white

@identifier = $letter $letter_or_digit*
@decimal = $digit+
@integer = \-? @decimal
@float = \-? @decimal \. @decimal

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

  "$" [$graphic \ ]                      { \s -> QuotedChar (s !! 1) }
  $binary_char+                          { \s -> BinarySelector s }
  @identifier                            { \s -> Identifier s }
  @identifier ":"                        { \s -> Keyword (init s) }
  $letter $letter_or_digit_or_colon* ":" { \s -> KeywordSelector s }
  @float                                 { \s -> Float (read s) }
  @integer                               { \s -> Integer (read s) }
  \" ($printable # \")* \"               { \s -> DoubleQuotedString (removeOuter 1 s) }
  \' ($printable # \')* \'               { \s -> SingleQuotedString (removeOuter 1 s) }
  \\ $letter $letter_or_digit*           { \s -> SingleQuotedString (tail s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - 1) (drop k x)
}
