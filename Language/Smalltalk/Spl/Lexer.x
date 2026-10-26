{
module Language.Smalltalk.Spl.Lexer where

import Music.Theory.List {- hmt-base -}
import Music.Theory.Read {- hmt-base -}

import Language.Smalltalk.Spl.Token
}

%wrapper "monad"

$digit = 0-9 -- 3.5.1
$lower = a-z -- 3.5.1 uppercaseAlphabetic
$upper = A-Z -- 3.5.1 lowercaseAlphabetic
$underscore = _ -- 3.5.1 nonCaseLetter
$letter = [a-z A-Z _] -- 3.5.1 letter
$letter_or_digit = [a-z A-Z _ 0-9]
$binary_char = [\! \@ \% \& \* \- \+ \= \| \< \> \? \/ \~ \^ \#] -- !@%&*-+=|<>?/~^#
$graphic = $printable # $white
$commentary = [a-z A-Z 0-9 \( \) \[ \] \' \" \` \. \; \= \, \: \_ \  \t \n \! \@ \% \& \* \- \+ \= \| \< \> \? \/ \~ \^ \# \$]

@lowercaseIdentifier = $lower $letter_or_digit* \!?
@uppercaseIdentifier = $upper $letter_or_digit*
@negatedIdentifier = \- @lowercaseIdentifier
@decimal = $digit+
@integer = \-? @decimal
@float = \-? @decimal \. @decimal

tokens :-

  <0> $white+                                ;
  <0> "/*"                                   { begin comment }
  <0> "*/"                                   { \_ _ -> alexError "Error: unexpected close comment" }

  <comment> "*/"                             { begin 0 }
  <comment> .                                ;
  <comment> \n                               ;

  <0> "("                                    { splToken LeftParen }
  <0> ")"                                    { splToken RightParen }
  <0> ","                                    { splToken Comma }
  <0> "."                                    { splToken Dot }
  <0> ".."                                   { splToken DotDot }
  <0> ":"                                    { splToken Colon }
  <0> "::"                                   { splToken ColonColon }
  <0> ":="                                   { splToken ColonEquals }
  <0> ";"                                    { splToken SemiColon }
  <0> "="                                    { splToken Equals }
  <0> "["                                    { splToken LeftBracket }
  <0> "]"                                    { splToken RightBracket }
  <0> "{"                                    { splToken LeftBrace }
  <0> "|"                                    { splToken VerticalBar }
  <0> "}"                                    { splToken RightBrace }

  <0> "false"                                { splToken FalseIdentifier }
  <0> "let"                                  { splToken Let }
  <0> "nil"                                  { splToken NilIdentifier }
  <0> "true"                                 { splToken TrueIdentifier }
  <0> "Infinity"                             { splToken InfinityIdentifier }
  <0> "NaN"                                  { splToken NaNIdentifier }

  <0> @lowercaseIdentifier                   { splStringToken LowercaseIdentifier }
  <0> @uppercaseIdentifier                   { splStringToken UppercaseIdentifier }
  <0> @negatedIdentifier                     { splStringToken NegatedIdentifier }
  <0> @lowercaseIdentifier "/" @integer      { splStringToken ArityQualifiedLowercaseIdentifier }
  <0> @uppercaseIdentifier "/" @integer      { splStringToken ArityQualifiedUppercaseIdentifier }
  <0> $binary_char+                          { splStringToken BinarySelector }
  <0> @float                                 { splStringToken (Float . read_err_msg "Float") }
  <0> @integer                               { splStringToken (Integer . read_err_msg "Integer") }
  <0> \" ($printable # \")* \"               { splStringToken (DoubleQuotedString . removeOuter 1) }
  <0> \' ($printable # \')* \'               { splStringToken (SingleQuotedString . removeOuter 1) }

{
removeOuter :: Int -> [t] -> [t]
removeOuter k x = take_err (length x - k - k) (drop_err k x)

alexEOF :: Alex Token
alexEOF = pure EOF

splToken :: Token -> AlexAction Token
splToken k _ _ = pure k

--byteToChar :: Byte -> Char
--byteToChar = toEnum . fromIntegral

splStringToken :: (String -> Token) -> AlexAction Token
splStringToken f (_, _, _, str) len =
  if len == 0
  then error "splStringToken: len=0"
  else pure (f (take_err len str))

alexMaybeScanTokens :: String -> Either String [Token]
alexMaybeScanTokens input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if output == EOF
        then pure [output]
        else (output :) <$> go

alexScanTokens :: String -> [Token]
alexScanTokens input =
  case alexMaybeScanTokens input of
    Left err -> error err
    Right answer -> answer
}
