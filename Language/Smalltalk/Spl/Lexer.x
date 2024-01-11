{
module Language.Smalltalk.Spl.Lexer where

import Language.Smalltalk.Spl.Token
}

%wrapper "basic"

$digit                 = 0-9                                  -- 3.5.1
$lower                 = a-z                                  -- 3.5.1 uppercaseAlphabetic
$upper                 = A-Z                                  -- 3.5.1 lowercaseAlphabetic
$underscore            = _                                    -- 3.5.1 nonCaseLetter
$letter                = [a-z A-Z _]                          -- 3.5.1 letter
$letterordigit         = [a-z A-Z _ 0-9]
$letterordigitorcolon  = [a-z A-Z _ 0-9 \:]
$binaryChar            = [\!\@\%\&\*\-\+\=\|\<\>\?\/\~\^]     -- !@%&*-+=|<>?/~
$graphic               = $printable # $white

@identifier            = $letter $letterordigit*
@decimal               = $digit+
@integer               = \-? @decimal
@float                 = \-? @decimal \. @decimal

tokens :-

  $white+                                ;
  "{-" ($printable # \})* "-}"           ;

  "["                                    { \_ -> LeftBracket }
  "]"                                    { \_ -> RightBracket }
  "."                                    { \_ -> Dot }
  ".."                                    { \_ -> DotDot }
  ":"                                    { \_ -> Colon }
  "::"                                   { \_ -> ColonColon }
  ";"                                    { \_ -> SemiColon }
  ","                                    { \_ -> Comma }
  "|"                                    { \_ -> VerticalBar }
  "{"                                    { \_ -> LeftBrace }
  "}"                                    { \_ -> RightBrace }
  "("                                    { \_ -> LeftParen }
  ")"                                    { \_ -> RightParen }

  "nil"                                  { \_ -> NilIdentifier }
  "true"                                 { \_ -> TrueIdentifier }
  "false"                                { \_ -> FalseIdentifier }
  "let"                                  { \_ -> Let }

  "="                                    { \_ -> EqualsOperator }
  ":="                                   { \_ -> AssignmentOperator }

  @identifier                            { \s -> Identifier s }
  @identifier ":/" @integer              { \s -> ArityQualifiedIdentifier s }
  $letter $letterordigit* ":"            { \s -> Keyword (init s) }
  $binaryChar+                           { \s -> BinarySelector s }
  @float                                 { \s -> Float (read s) }
  @integer                               { \s -> Integer (read s) }
  \" ($printable # \")* \"               { \s -> DoubleQuotedString (removeOuter 1 s) }
  \' ($printable # \')* \'               { \s -> SingleQuotedString (removeOuter 1 s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - k) (drop k x)
}
