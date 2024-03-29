{
module Language.Smalltalk.Ansi.Parser.Happy.Lexer where

import Language.Smalltalk.Ansi.Parser.Happy.Token

}

%wrapper "basic"

$digit         = 0-9                                  -- 3.5.1
$lower         = a-z                                  -- 3.5.1 uppercaseAlphabetic
$upper         = A-Z                                  -- 3.5.1 lowercaseAlphabetic
$underscore    = _                                    -- 3.5.1 nonCaseLetter
$letter        = [a-z A-Z _]                          -- 3.5.1 letter
$letterordigit = [a-z A-Z _ 0-9]
$binaryChar    = [\!\%\&\*\+\,\/\<\=\>\?\@\\\~\|\-] -- 3.5.5 (!%&*+,/<=>?@\~|-)
$graphic       = $printable # $white

@decimal     = $digit+
@exponent    = e \-? @decimal
@integer     = \-? @decimal
@float       = \-? @decimal \. @decimal @exponent?
@quotequote  = \' \'
@string      = $printable # \' | @quotequote

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
  "<"                                   { \_ -> LeftAngleBracket }
  ">"                                   { \_ -> RightAngleBracket }

  "nil"                                 { \_ -> NilIdentifier }
  "true"                                { \_ -> TrueIdentifier }
  "false"                               { \_ -> FalseIdentifier }
  "self"                                { \_ -> SelfIdentifier }
  "super"                               { \_ -> SuperIdentifier }
  "primitive                            { \_ -> PrimitiveIdentifier }

  $letter $letterordigit*               { \s -> Identifier s }
  $letter $letterordigit* ":"           { \s -> Keyword s }
  $binaryChar+                          { \s -> BinarySelector s }
  "^"                                   { \_ -> ReturnOperator }
  ":="                                  { \_ -> AssignmentOperator }
  @float | @decimal @exponent           { \s -> Float (read s) }
  @integer                              { \s -> Integer (read s) }
  "$" [$graphic \ ]                     { \s -> QuotedChar (s !! 1) }
  \' @string* \'                        { \s -> QuotedString (removeOuter 1 s) }
  \# \' @string* \'                     { \s -> HashedString (removeOuter 2 s) }

{
-- | Remove k items from the start and one item from the end.
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take (length x - k - 1) (drop k x)

smalltalkLexerTest = do
  s <- getContents
  print (alexScanTokens s)
}
