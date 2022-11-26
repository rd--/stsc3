{
module Language.Smalltalk.SuperCollider.Lexer where

import Language.Smalltalk.SuperCollider.Token
}

%wrapper "basic"

$digit                 = 0-9                                  -- 3.5.1
$lower                 = a-z                                  -- 3.5.1 uppercaseAlphabetic
$upper                 = A-Z                                  -- 3.5.1 lowercaseAlphabetic
$underscore            = _                                    -- 3.5.1 nonCaseLetter
$letter                = [a-z A-Z _]                          -- 3.5.1 letter
$letterordigit         = [a-z A-Z _ 0-9]
$letterordigitorcolon  = [a-z A-Z _ 0-9 \:]
$binaryChar            = [\!\@\%\&\*\-\+\=\|\<\>\?\/]         -- !@%&*-+=|<>?/
$graphic               = $printable # $white

@decimal               = $digit+
@integer               = \-? @decimal
@float                 = \-? @decimal \. @decimal

tokens :-

  $white+                                ;
  ";;" $printable+                       ;

  "["                                    { \_ -> LeftBracket }
  "]"                                    { \_ -> RightBracket }
  "."                                    { \_ -> Dot }
  ".."                                    { \_ -> DotDot }
  ":"                                    { \_ -> Colon }
  ";"                                    { \_ -> SemiColon }
  ","                                    { \_ -> Comma }
  "{"                                    { \_ -> LeftBrace }
  "}"                                    { \_ -> RightBrace }
  "("                                    { \_ -> LeftParen }
  ")"                                    { \_ -> RightParen }
  "#["                                   { \_ -> HashLeftBracket }

  "nil"                                  { \_ -> NilIdentifier }
  "true"                                 { \_ -> TrueIdentifier }
  "false"                                { \_ -> FalseIdentifier }
  "self"                                 { \_ -> SelfIdentifier }
  "arg"                                  { \_ -> Arg }
  "var"                                  { \_ -> Var }
  "classvar"                             { \_ -> ClassVar }

  "^"                                    { \_ -> ReturnOperator }
  "="                                    { \_ -> EqualsOperator }
  ":="                                   { \_ -> AssignmentOperator }
  "*"                                    { \_ -> ClassMethodOperator }
  "+"                                    { \_ -> ClassExtensionOperator }

  $letter $letterordigit*                { \s -> Identifier s }
  $letter $letterordigit* ":"            { \s -> Keyword (init s) }
  $letter $letterordigitorcolon* ":"     { \s -> KeywordSelector s }
  $binaryChar+                           { \s -> BinarySelector s }
  @float                                 { \s -> Float (read s) }
  @integer                               { \s -> Integer (read s) }
  "$" [$graphic \ ]                      { \s -> QuotedChar (s !! 1) }
  \" ($printable # \")* \"               { \s -> QuotedString (removeOuter 1 s) }
  \' ($printable # \')* \'               { \s -> HashedString (removeOuter 1 s) }
  \\ $letter $letterordigit*             { \s -> HashedString (tail s) }
  "(*" ($printable # \*)* "*)"           { \s -> Comment (removeOuter 2 s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - 1) (drop k x)
}
