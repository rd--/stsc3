{- | Printer from Ansi Smalltalk Ast to .stc / .spl

Notes:

- Translations are: self->this =->== ~=->!= ,->++
- Keyword selectors are rewritten to
  - remove : characters
  - start interior words with upper case letters
  - end with a _
  - (ie. at:put: -> atPut_)
- Names that begin with an upper case letter that would be invalid in Sc are rewritten
  - class variable, unary and keyword names, assignment targets
  - if the Smalltalk file contains such names the translation may contain errors, i.e. ^Name would not be rewritten
-}
module Language.Smalltalk.Ansi.Print.Spl where

import Data.Char {- base -}
import Data.List {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import qualified Music.Theory.List as List {- hmt-base -}

import Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Ansi.Print as Ansi.Print {- stsc3 -}

strjnComma :: [String] -> String
strjnComma = Ansi.Print.strjnWith ','

sc_smalltalkProgram_pp :: Bool -> St.SmalltalkProgram -> String
sc_smalltalkProgram_pp spl = unlines . map (sc_programElement_pp spl) . St.programElements

sc_programElement_pp :: Bool -> St.ProgramElement -> String
sc_programElement_pp spl el =
  case el of
    St.ProgramGlobal g -> sc_globalDefinition_pp spl g
    St.ProgramInitializer i -> sc_programInitializerDefinition_pp spl i

isInstanceVar :: St.ClassDefinition -> St.Identifier -> Bool
isInstanceVar c nm = nm `elem` St.classInstanceVariableNames c

removeTrailingColon :: St.Identifier -> St.Identifier
removeTrailingColon x = if last x == ':' then take (length x - 1) x else x

isGetterOrSetter :: St.ClassDefinition -> St.MethodDefinition -> Bool
isGetterOrSetter c m = isInstanceVar c (removeTrailingColon (St.selectorIdentifier (St.methodSelector m)))

{- | TitleCase to camelCase

>>> downcaseFirstLetter "VariableName"
"variableName"
-}
downcaseFirstLetter :: String -> String
downcaseFirstLetter s =
  case s of
    l : s' -> (if isUpper l then toLower l else l) : s'
    _ -> s

makeVariablePublic :: String -> String
makeVariablePublic = ("<>" ++)

sc_ClassDefinition_pp :: Bool -> St.ClassDefinition -> String
sc_ClassDefinition_pp spl c =
  let l f x = if null x then "" else f x
  in unlines
      [ printf "%s %s {" (St.className c) (maybe "" (\x -> printf " : %s" x) (St.superclassName c))
      , l (\x -> printf (if spl then "| %s |" else "var %s;") (Ansi.Print.strjnWith (if spl then ' ' else ',') x)) (St.classInstanceVariableNames c)
      , l (\x -> printf "classvar %s;" (strjnComma (map downcaseFirstLetter x))) (St.classVariableNames c)
      , unlines (map (sc_methodDefinition_pp spl Nothing) (St.instanceMethods c))
      , unlines (map (sc_methodDefinition_pp spl (Just '*')) (St.classMethods c))
      , "}"
      ]

sc_globalDefinition_pp :: Bool -> St.GlobalDefinition -> String
sc_globalDefinition_pp spl (St.GlobalDefinition n i) = Ansi.Print.strjn [n, maybe "" (sc_variableInitializer_pp spl) i]

sc_variableInitializer_pp :: Bool -> St.VariableInitializer -> String
sc_variableInitializer_pp spl = sc_initializerDefinition_pp spl

sc_programInitializerDefinition_pp :: Bool -> St.ProgramInitializerDefinition -> String
sc_programInitializerDefinition_pp spl = sc_initializerDefinition_pp spl

{- | Translate keyword

>>> sc_keywordSelector False ["at:","put:"]
"atPut_"

>>> sc_keywordSelector False ["Required"]
"required_"
-}
sc_keywordSelector :: Bool -> [St.Identifier] -> St.Identifier
sc_keywordSelector spl k =
  let remcadd_ s = downcaseFirstLetter (filter (/= ':') s ++ (if spl then "" else "_"))
      cap s = if length s > 1 then (toUpper (List.head_err s)) : List.tail_err s else s
  in case k of
      [] -> error "sc_keywordSelector?"
      [k0] -> remcadd_ (downcaseFirstLetter k0)
      k0 : kN -> remcadd_ (downcaseFirstLetter k0 ++ concatMap cap kN)

{- | This rewrites the symbols "=" (to "==") "~=" (to "!=") and "," (to "++").
     These are required for translation.
     Other rewriting is defered.
-}
sc_binop_rewrite :: Bool -> St.BinaryIdentifier -> St.BinaryIdentifier
sc_binop_rewrite spl b =
  case b of
    "=" -> if spl then "=" else "=="
    "==" -> if spl then "==" else "==="
    "~=" -> if spl then "~=" else "!="
    "," -> "++"
    _ -> b

{- | Translate Pattern

>>> sc_patternSelector False (stParse messagePattern "= x")
"=="

>>> sc_patternSelector False (stParse messagePattern "Required")
"required"
-}
sc_patternSelector :: Bool -> St.Pattern -> St.Identifier
sc_patternSelector spl pat =
  case pat of
    St.UnaryPattern u -> downcaseFirstLetter u
    St.BinaryPattern b _ -> sc_binop_rewrite spl b
    St.KeywordPattern kp -> sc_keywordSelector spl (map fst kp)

sc_methodDefinition_pp :: Bool -> Maybe Char -> St.MethodDefinition -> String
sc_methodDefinition_pp spl maybeStar (St.MethodDefinition _ _ pat tmp stm prm _ _) =
  Ansi.Print.strjn
    [ maybe "" return maybeStar
    , sc_patternSelector spl pat
    , "{"
    , sc_patternArgs_pp spl pat
    , maybe "" Ansi.Print.primitive_pp prm
    , maybe "" (sc_temporaries_pp spl) tmp
    , maybe "" (sc_statements_pp spl) stm
    , "}"
    ]

sc_patternArgs_pp :: Bool -> St.Pattern -> String
sc_patternArgs_pp spl pat =
  case pat of
    St.UnaryPattern _ -> ""
    St.BinaryPattern _ a -> printf (if spl then " :%s |" else "arg %s;") a
    St.KeywordPattern kp ->
      if spl
        then printf " %s |" (Ansi.Print.strjn (map ((":" ++) . snd) kp))
        else printf "arg %s;" (strjnComma (map snd kp))

sc_temporaries_pp :: Bool -> St.Temporaries -> String
sc_temporaries_pp spl (St.Temporaries t) =
  if spl
    then printf "| %s |" (Ansi.Print.strjn t)
    else printf "var %s;" (strjnComma t)

sc_comment_pp :: St.Comment -> String
sc_comment_pp x = "(* " ++ x ++ " *)"

sc_initializerDefinition_pp :: Bool -> St.InitializerDefinition -> String
sc_initializerDefinition_pp spl (St.InitializerDefinition c t s) =
  Ansi.Print.strjnWith
    ';'
    [ maybe "" sc_comment_pp c
    , maybe "" (sc_temporaries_pp spl) t
    , maybe "" (sc_statements_pp spl) s
    ]

pp_inChars :: (Char, Char) -> String -> String
pp_inChars (x, y) s = x : s ++ [y]

pp_inBraces :: String -> String
pp_inBraces = pp_inChars ('{', '}')

{- | Translate block

>>> let p spl = sc_blockBody_pp spl . stParse blockBody
>>> p True ""
"{}"

>>> p True "x"
"{x}"

>>> p True "x * 2"
"{x * 2}"

>>> p True ":x | x * 2]"
"{ :x | x * 2}"

>>> p False ":x | x * 2]"
"{arg x; x * 2}"

>>> p True ":x :y | (x * x) + (y * y)]"
"{ :x :y | (x * x) + (y * y)}"

>>> p False ":x :y | (x * x) + (y * y)]"
"{arg x,y; (x * x) + (y * y)}"
-}
sc_blockBody_pp :: Bool -> St.BlockBody -> String
sc_blockBody_pp spl (St.BlockBody _ a t s) =
  pp_inBraces $
    Ansi.Print.strjn
      [ maybe
          ""
          ( printf (if spl then " %s |" else "arg %s;")
              . (if spl then Ansi.Print.strjn else strjnComma)
              . map (sc_blockArgument_pp spl)
          )
          a
      , maybe "" (sc_temporaries_pp spl) t
      , maybe "" (sc_statements_pp spl) s
      ]

sc_blockArgument_pp :: Bool -> St.BlockArgument -> String
sc_blockArgument_pp spl = if spl then (":" ++) else id

sc_statements_pp :: Bool -> St.Statements -> String
sc_statements_pp spl st =
  case st of
    St.StatementsReturn r -> sc_returnStatement_pp spl r
    St.StatementsExpression e st' -> Ansi.Print.strjn [sc_expression_pp spl e, maybe "" ((";" ++) . sc_statements_pp spl) st']

{- | Print ReturnStatement

>>> let p = sc_returnStatement_pp False . stParse returnStatement
>>> p "^self"
"^this"
-}
sc_returnStatement_pp :: Bool -> St.ReturnStatement -> String
sc_returnStatement_pp spl (St.ReturnStatement e) =
  if spl
    then
      let isNonLocalReturn = False
      in if isNonLocalReturn
          then printf "%s.return" (sc_expression_pp spl e)
          else sc_expression_pp spl e
    else printf "^%s" (sc_expression_pp spl e)

sc_expression_pp :: Bool -> St.Expression -> String
sc_expression_pp spl = St.expressionCase (sc_assignment_pp spl) (sc_basicExpression_pp spl)

sc_assignment_pp :: Bool -> St.Assignment -> String
sc_assignment_pp spl (St.Assignment i e) = printf "%s %s %s" (downcaseFirstLetter i) (if spl then ":=" else "=") (sc_expression_pp spl e)

-- | Decide if Messages require the binary sequence to be parenthesised.
requiresParen :: Maybe St.Messages -> Bool
requiresParen m =
  case m of
    Just (St.MessagesUnary _ (Just _) (Just _)) -> True
    Just (St.MessagesBinary _ (Just _)) -> True
    _ -> False

{- | Binary messages must be parenthesised because keyword messages have higher precedence.

>>> let p = sc_basicExpression_pp False . stParse basicExpression
>>> p "x + y"
"x + y"

>>> p "x + y * z"
"x + y * z"

>>> p "x y + z q"
"x .y + z .q"

>>> p "x y + z min: a"
"( x .y + z ) .min_(a)"

>>> p "x * y + z min: 3"
"( x * y + z ) .min_(3)"

>>> p "x == 0 ifTrue: [y]"
"( x === 0 ) .ifTrue_({y})"

>>> p "Strength SymPreferred"
"Strength .symPreferred"
-}
sc_basicExpression_pp :: Bool -> St.BasicExpression -> String
sc_basicExpression_pp spl (St.BasicExpression p m c) =
  let rqp = requiresParen m
  in Ansi.Print.strjn
      [ if rqp then "(" else ""
      , sc_primary_pp spl p
      , maybe "" (sc_messages_pp spl rqp) m
      , maybe "" sc_cascadedMessages_pp c
      ]

{- | In .stc keyword patterns bind more closely than binary patterns.

>>> let p = sc_messages_pp False True . stParse messages
>>> p "min: 2 * 3"
".min_(2 * 3)"

>>> p "at: 0 put: x * 2"
".atPut_(0,x * 2)"

> p "== 0 ifTrue: [error]" -- the initial binary operator needs to be parenthesised....
-}
sc_messages_pp :: Bool -> Bool -> St.Messages -> String
sc_messages_pp spl rqp ms =
  case ms of
    St.MessagesUnary m1 m2 m3 ->
      Ansi.Print.strjn
        ( concat
            ( [ map sc_unaryMessage_pp m1
              , maybe [] (map (sc_binaryMessage_pp spl)) m2
              , if rqp then [")"] else []
              , maybe [] (return . sc_keywordMessage_pp spl) m3
              ]
            )
        )
    St.MessagesBinary m1 m2 ->
      Ansi.Print.strjn
        ( concat
            ( [ map (sc_binaryMessage_pp spl) m1
              , if rqp then [")"] else []
              , maybe [] (return . sc_keywordMessage_pp spl) m2
              ]
            )
        )
    St.MessagesKeyword m1 ->
      sc_keywordMessage_pp spl m1

{- | This rewrites the identifiers "self" (to "this").
     This is required for translation.
     Other rewriting is defered.

>>> sc_primary_pp False (stParse primary "self")
"this"
-}
sc_primary_pp :: Bool -> St.Primary -> String
sc_primary_pp spl pr =
  case pr of
    St.PrimaryIdentifier i ->
      case i of
        "self" -> if spl then "self" else "this"
        _ -> i
    St.PrimaryLiteral l -> sc_literal_pp l
    St.PrimaryBlock b -> sc_blockBody_pp spl b
    St.PrimaryExpression e -> printf "(%s)" (sc_expression_pp spl e)
    St.PrimaryArrayExpression a -> printf "{%s}" (intercalate " . " (map (sc_basicExpression_pp spl) a))

{- | Translate unary message

>>> sc_unaryMessage_pp (stParse unaryMessage "abs")
".abs"

>>> sc_unaryMessage_pp (stParse unaryMessage "Required")
".required"
-}
sc_unaryMessage_pp :: St.UnaryMessage -> String
sc_unaryMessage_pp = ('.' :) . downcaseFirstLetter . St.selectorIdentifier . St.unaryMessageSelector

{- | Translate binary message

>>> sc_binaryMessage_pp False (stParse binaryMessage "= 0")
"== 0"

>>> sc_binaryMessage_pp False (stParse binaryMessage "+ 2")
"+ 2"
-}
sc_binaryMessage_pp :: Bool -> St.BinaryMessage -> String
sc_binaryMessage_pp spl (St.BinaryMessage b a) = Ansi.Print.strjn [sc_binop_rewrite spl b, sc_binaryArgument_pp spl a]

{- | Translate keyword message

>>> sc_keywordMessage_pp False (stParse keywordMessage "at: 0")
".at_(0)"

>>> sc_keywordMessage_pp False (stParse keywordMessage "at: 0 put: x")
".atPut_(0,x)"
-}
sc_keywordMessage_pp :: Bool -> St.KeywordMessage -> String
sc_keywordMessage_pp spl (St.KeywordMessage l) =
  let (ks, as) = unzip l
  in printf ".%s(%s)" (sc_keywordSelector spl ks) (strjnComma (map (sc_keywordArgument_pp spl) as))

sc_binaryArgument_pp :: Bool -> St.BinaryArgument -> String
sc_binaryArgument_pp spl (St.BinaryArgument p m) =
  Ansi.Print.strjn [sc_primary_pp spl p, maybe "" (Ansi.Print.strjn . map sc_unaryMessage_pp) m]

sc_keywordArgument_pp :: Bool -> St.KeywordArgument -> String
sc_keywordArgument_pp spl (St.KeywordArgument p m1 m2) =
  Ansi.Print.strjn
    [ sc_primary_pp spl p
    , maybe "" (Ansi.Print.strjn . map sc_unaryMessage_pp) m1
    , maybe "" (Ansi.Print.strjn . map (sc_binaryMessage_pp spl)) m2
    ]

sc_cascadedMessages_pp :: St.CascadedMessages -> String
sc_cascadedMessages_pp = error "CascadedMessages?"

sc_literal_pp :: St.Literal -> String
sc_literal_pp lit =
  case lit of
    St.NumberLiteral n -> Ansi.Print.number_pp n
    St.StringLiteral s -> printf "\"%s\"" s
    St.CharacterLiteral c -> printf "$%c" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral s -> printf "\\%s" (Ansi.Print.selector_pp s)
    St.ArrayLiteral a -> printf "#[%s]" (Ansi.Print.strjnWith ',' (map (either sc_literal_pp id) a))

sc_number_pp :: St.Number -> String
sc_number_pp = St.numberCase show (\x -> Numeric.showFFloat Nothing x "")

sc_quotedCharacter_pp :: St.QuotedCharacter -> String
sc_quotedCharacter_pp = printf "$%c"

sc_quotedString_pp :: St.QuotedString -> String
sc_quotedString_pp = printf "\"%s\""

sc_hashedString_pp :: St.HashedString -> String
sc_hashedString_pp = printf "'%s'"

sc_selector_pp :: St.Selector -> String
sc_selector_pp sel =
  case sel of
    St.UnarySelector u -> u
    St.BinarySelector b -> b
    St.KeywordSelector k _ -> k
