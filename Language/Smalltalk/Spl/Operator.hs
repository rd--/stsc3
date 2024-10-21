module Language.Smalltalk.Spl.Operator where

import qualified Data.Char

import qualified Music.Theory.List as List {- hmt-base -}

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter x =
  case x of
    c:x' -> Data.Char.toUpper c : x'
    _ -> x

{- | Operator and other non-letter characters.

The characters :;.,()[]{}`"'_ aren't allowed in operator names.
They're here so help file lookup can find them.
-}
operatorCharacters :: [Char]
operatorCharacters = "+*-/&|@<>=%!\\~?^#$:;.,()[]{}`\"\'"

isOperatorName :: String -> Bool
isOperatorName name =
  case name of
    c:_ -> c `elem` operatorCharacters
    _ -> False

operatorCharacterNameTable :: [(Char, String)]
operatorCharacterNameTable =
  [('!', "exclamationMark") -- U+0021 ! Exclamation Mark
  ,('"', "quotationMark") -- U+0022 " Quotation Mark
  ,('#', "numberSign") -- U+0023 # Number Sign
  ,('$', "dollarSign") -- U+0024 $ Dollar Sign
  ,('%', "percentSign") -- U+0025 % Percent Sign
  ,('&', "ampersand") -- U+0026 & Ampersand
  ,('\'', "apostrophe") -- U+0027 ' Apostrophe
  ,('(', "leftParenthesis") -- U+0028 ( Left Parenthesis
  ,(')', "rightParenthesis") -- U+0029 ) Right Parenthesis
  ,('*', "asterisk") -- U+002a * Asterisk
  ,('+', "plusSign") -- U+002b + Plus Sign
  ,(',', "comma") -- U+002C , Comma
  ,('-', "hyphenMinus") -- U+002d - Hyphen-Minus
  ,(':', "colon") -- U+003a : Colon
  ,(';', "semicolon") -- U+003b ; Semicolon
  ,('<', "lessThanSign") -- U+003c < Less-Than Sign
  ,('=', "equalsSign") -- U+003d = Equals Sign
  ,('>', "greaterThanSign") -- U+003e > Greater-Than Sign
  ,('.', "fullStop") -- U+002e . Full Stop
  ,('/', "solidus") -- U+002f / Solidus
  ,('?', "questionMark") -- U+003f ? Question Mark
  ,('@', "commercialAt") -- U+0040 @ Commercial At
  ,('[', "leftSquareBracket") -- U+005B [ Left Square Bracket
  ,('\\', "reverseSolidus") -- U+005c \ Reverse Solidus
  ,(']', "rightSquareBracket") -- U+005D ] Right Square Bracket
  ,('^', "circumflexAccent") -- U+005e ^ Circumflex Accent
  ,('_', "lowLine") -- Unicode U+005F _ Low Line
  ,('`', "graveAccent") -- U+0060 ` Grave Accent
  ,('{', "leftCurlyBracket") -- U+007B { Left Curly Bracket
  ,('|', "verticalLine") -- - U+007c | Vertical Line
  ,('}', "rightCurlyBracket") -- U+007D } Right Curly Bracket
  ,('~', "tilde") -- U+007e ~ Tilde
  ]

operatorMethodName :: String -> String
operatorMethodName name =
  let operatorWords = map (\c -> List.lookup_err c operatorCharacterNameTable) name
      capilatizedWords = case operatorWords of
                           x:xs -> x : map capitalizeFirstLetter xs
                           _ -> operatorWords
  in concat capilatizedWords

{- | Resolve method name, converting operator name to method name if required.

>>> resolveMethodName "++"
"plusSignPlusSign"

>>> resolveMethodName ">="
"greaterThanSignEqualsSign"

>>> resolveMethodName "collect"
"collect"
-}
resolveMethodName :: String -> String
resolveMethodName name =
  if isOperatorName name
  then operatorMethodName name
  else name
