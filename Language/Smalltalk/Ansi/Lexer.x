{
module Language.Smalltalk.Ansi.Lexer where

import Language.Smalltalk.Ansi.Token

}

%wrapper "basic"

$digit         = 0-9                                  -- 3.5.1
$lower         = a-z                                  -- 3.5.1 uppercaseAlphabetic
$upper         = A-Z                                  -- 3.5.1 lowercaseAlphabetic
$underscore    = _                                    -- 3.5.1 nonCaseLetter
$letter        = [a-z A-Z _]                          -- 3.5.1 letter
$letterordigit = [a-z A-Z _ 0-9]
$binaryChar    = [\!\%\&\*\+\,\/\>\<\=\>\?\@\\\~\|\-] -- 3.5.5
$graphic       = $printable # $white

@decimal     = $digit+
@integer     = \-? @decimal
@float       = \-? @decimal \. @decimal

tokens :-

  $white+                               ;
  \" ([$printable \n] # \")+ \"         ;

  "|"                                   { \_ -> VerticalBar }
  "["                                   { \_ -> LeftBracket }
  "]"                                   { \_ -> RightBracket }
  "."                                   { \_ -> Dot }
  ":"                                   { \_ -> Colon }
  ";"                                   { \_ -> SemiColon }
  "{"                                   { \_ -> LeftBrace }
  "}"                                   { \_ -> RightBrace }
  "#"                                   { \_ -> Hash }
  "#("                                  { \_ -> HashLeftParen }
  "("                                   { \_ -> LeftParen }
  ")"                                   { \_ -> RightParen }

  "nil"                                 { \_ -> NilIdentifier }
  "true"                                { \_ -> TrueIdentifier }
  "false"                               { \_ -> FalseIdentifier }
  "self"                                { \_ -> SelfIdentifier }
  "super"                               { \_ -> SuperIdentifier }

  $letter $letterordigit*               { \s -> Identifier s }
  $letter $letterordigit* ":"           { \s -> Keyword s }
  $binaryChar+                          { \s -> BinarySelector s }
  "^"                                   { \_ -> ReturnOperator }
  ":="                                  { \_ -> AssignmentOperator }
  @float                                { \s -> Float (read s) }
  @integer                              { \s -> Integer (read s) }
  "$" $graphic                          { \s -> QuotedChar (s !! 1) }
  \' ($printable # \')* \'              { \s -> QuotedString (removeOuter 1 s) }
  \# \' ($printable # \')* \'           { \s -> HashedString (removeOuter 2 s) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - 1) (drop k x)

smalltalkLexerTest = do
  s <- getContents
  print (alexScanTokens s)
}
