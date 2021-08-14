{- | Printer from ANSI Smalltalk AST to SuperCollider

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
module Language.Smalltalk.Ansi.Print.SuperCollider where

import Data.Char {- base -}
import Data.List {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Ansi.Print as St {- stsc3 -}

strjnComma :: [String] -> String
strjnComma = strjnWith ','

sc_smalltalkProgram_pp :: SmalltalkProgram -> String
sc_smalltalkProgram_pp = unlines . map sc_programElement_pp . programElements

sc_programElement_pp :: ProgramElement -> String
sc_programElement_pp el =
  case el of
    ProgramGlobal g -> sc_globalDefinition_pp g
    ProgramInitializer i -> sc_programInitializerDefinition_pp i

isInstanceVar :: ClassDefinition -> Identifier -> Bool
isInstanceVar c nm = nm `elem` classInstanceVariableNames c

removeTrailingColon :: Identifier -> Identifier
removeTrailingColon x = if last x == ':' then take (length x - 1) x else x

isGetterOrSetter :: ClassDefinition -> MethodDefinition -> Bool
isGetterOrSetter c m = isInstanceVar c (removeTrailingColon (selectorIdentifier (methodSelector m)))

-- > downcaseFirstLetter "VariableName" == "variableName"
downcaseFirstLetter :: String -> String
downcaseFirstLetter s =
  case s of
    l:s' -> (if isUpper l then toLower l else l) : s'
    _ -> s

makeVariablePublic :: String -> String
makeVariablePublic = ("<>" ++)

sc_ClassDefinition_pp :: ClassDefinition -> String
sc_ClassDefinition_pp c =
  let l f x = if null x then "" else f x
  in unlines
     [printf "%s %s {" (className c) (maybe "" (\x -> printf " : %s" x) (superclassName c))
     ,l (\x -> printf "var %s;" (strjnComma x)) (classInstanceVariableNames c)
     ,l (\x -> printf "classvar %s;" (strjnComma (map downcaseFirstLetter x))) (classVariableNames c)
     ,unlines (map (sc_methodDefinition_pp Nothing) (instanceMethods c))
     ,unlines (map (sc_methodDefinition_pp (Just '*')) (classMethods c))
     ,"}"]

sc_globalDefinition_pp :: GlobalDefinition -> String
sc_globalDefinition_pp (GlobalDefinition n i) = strjn [n,maybe "" sc_variableInitializer_pp i]

sc_variableInitializer_pp :: VariableInitializer -> String
sc_variableInitializer_pp = sc_initializerDefinition_pp

sc_programInitializerDefinition_pp :: ProgramInitializerDefinition -> String
sc_programInitializerDefinition_pp = sc_initializerDefinition_pp

-- > sc_keywordSelector ["at:","put:"] == "atPut_"
-- > sc_keywordSelector ["Required"] == "required_"
sc_keywordSelector :: [Identifier] -> Identifier
sc_keywordSelector k =
  let remcadd_ s = downcaseFirstLetter (filter (/= ':') s ++ "_")
      cap s = if length s > 1 then (toUpper (head s)) : tail s else s
  in case k of
       [] -> error "sc_keywordSelector?"
       [k0] -> remcadd_ (downcaseFirstLetter k0)
       k0:kN -> remcadd_ (downcaseFirstLetter k0 ++ concatMap cap kN)

{- | This rewrites the symbols "=" (to "==") "~=" (to "!=") and "," (to "++").
     These are required for translation.
     Other rewriting is defered.
-}
sc_binop_rewrite :: BinaryIdentifier -> BinaryIdentifier
sc_binop_rewrite b =
  case b of
    "=" -> "=="
    "~=" -> "!="
    "," -> "++"
    _ -> b

-- > sc_patternSelector (stParse messagePattern "= x") == "=="
-- > sc_patternSelector (stParse messagePattern "Required") == "required"
sc_patternSelector :: Pattern -> Identifier
sc_patternSelector pat =
  case pat of
    UnaryPattern u -> downcaseFirstLetter u
    BinaryPattern b _ -> sc_binop_rewrite b
    KeywordPattern kp -> sc_keywordSelector (map fst kp)

sc_methodDefinition_pp :: Maybe Char -> MethodDefinition -> String
sc_methodDefinition_pp maybeStar (MethodDefinition _ _ p t s) =
  strjn [maybe "" return maybeStar
        ,sc_patternSelector p
        ,"{"
        ,sc_patternArgs_pp p
        ,maybe "" sc_temporaries_pp t
        ,maybe "" sc_statements_pp s
        ,"}"]

sc_patternArgs_pp :: Pattern -> String
sc_patternArgs_pp pat =
  case pat of
    UnaryPattern _ -> ""
    BinaryPattern _ a -> printf "arg %s;" a
    KeywordPattern kp -> printf "arg %s;" (strjnComma (map snd kp))

sc_temporaries_pp :: Temporaries -> String
sc_temporaries_pp (Temporaries t) = printf "var %s;" (strjnComma t)

sc_initializerDefinition_pp :: InitializerDefinition -> String
sc_initializerDefinition_pp (InitializerDefinition t s) =
  strjnWith ';' [maybe "" sc_temporaries_pp t,maybe "" sc_statements_pp s]

pp_inChars :: (Char,Char) -> String -> String
pp_inChars (x,y) s = x : s ++ [y]

pp_inBraces :: String -> String
pp_inBraces = pp_inChars ('{','}')

{-
> let p = sc_blockBody_pp . stParse blockBody
> p ""
> p "x"
> p "x * 2"
> p ":x | x * 2]"
> p ":x :y | (x * x) + (y * y)]"
-}
sc_blockBody_pp :: BlockBody -> String
sc_blockBody_pp (BlockBody a t s) =
  pp_inBraces$
  strjn [maybe "" (printf "arg %s;" . strjnComma . map sc_blockArgument_pp) a
        ,maybe "" sc_temporaries_pp t
        ,maybe "" sc_statements_pp s]

sc_blockArgument_pp :: BlockArgument -> String
sc_blockArgument_pp = id

sc_statements_pp :: Statements -> String
sc_statements_pp st =
  case st of
    StatementsReturn r -> sc_returnStatement_pp r
    StatementsExpression e st' -> strjn [sc_expression_pp e,maybe "" ((";" ++) . sc_statements_pp) st']

{- | Print ReturnStatement

p = sc_returnStatement_pp . stParse returnStatement
p "^self" == "^this"
-}
sc_returnStatement_pp :: ReturnStatement -> String
sc_returnStatement_pp (ReturnStatement e) = printf "^%s" (sc_expression_pp e)

sc_expression_pp :: Expression -> String
sc_expression_pp = expressionEither sc_assignment_pp sc_basicExpression_pp

sc_assignment_pp :: Assignment -> String
sc_assignment_pp (Assignment i e) = printf "%s = %s" (downcaseFirstLetter i) (sc_expression_pp e)

-- | Decide if Messages require the binary sequence to be parenthesised.
requiresParen :: Maybe Messages -> Bool
requiresParen m =
  case m of
    Just (MessagesUnary _ (Just _) (Just _)) -> True
    Just (MessagesBinary _ (Just _)) -> True
    _ -> False

{- | Binary messages must be parenthesised because keyword messages have higher precedence.

> let p = sc_basicExpression_pp . stParse basicExpression
> p "x + y" == "x + y"
> p "x + y * z" == "x + y * z"
> p "x y + z q" == "x .y + z .q"
> p "x y + z min: a" == "( x .y + z ) .min(a)"
> p "x * y + z min: 3" == "( x * y + z ) .min(3)"
> p "x == 0 ifTrue: [y]" == "( x == 0 ) .ifTrue({y})"
> p "Strength SymPreferred"
-}
sc_basicExpression_pp :: BasicExpression -> String
sc_basicExpression_pp (BasicExpression p m c) =
  let rqp = requiresParen m
  in strjn [if rqp then "(" else ""
           ,sc_primary_pp p
           ,maybe "" (sc_messages_pp rqp) m
           ,maybe "" sc_cascadedMessages_pp c]

{- | In SuperCollider keyword patterns bind more closely than binary patterns.

> let p = sc_messages_pp . stParse messages
> p "min: 2 * 3"
> p "at: 0 put: x * 2"
> p "== 0 ifTrue: [error]" -- the initial binary operator needs to be parenthesised....
-}
sc_messages_pp :: Bool -> Messages -> String
sc_messages_pp rqp ms =
  case ms of
    MessagesUnary m1 m2 m3 -> strjn (concat ([map sc_unaryMessage_pp m1
                                             ,maybe [] (map sc_binaryMessage_pp) m2
                                             ,if rqp then [")"] else []
                                             ,maybe [] (return . sc_keywordMessage_pp) m3]))
    MessagesBinary m1 m2 -> strjn (concat ([map sc_binaryMessage_pp m1
                                           ,if rqp then [")"] else []
                                           ,maybe [] (return . sc_keywordMessage_pp) m2]))
    MessagesKeyword m1 -> sc_keywordMessage_pp m1

{- | This rewrites the identifiers "self" (to "this").
     This is required for translation.
     Other rewriting is defered.

> sc_primary_pp (stParse primary "self") == "this"
-}
sc_primary_pp :: Primary -> String
sc_primary_pp pr =
  case pr of
    PrimaryIdentifier i -> case i of
                             "self" -> "this"
                             _ -> i
    PrimaryLiteral l -> sc_literal_pp l
    PrimaryBlock b -> sc_blockBody_pp b
    PrimaryExpression e -> printf "(%s)" (sc_expression_pp e)
    PrimaryArrayExpression a -> printf "{%s}" (intercalate " . " (map sc_basicExpression_pp a))

-- > sc_unaryMessage_pp (stParse unaryMessage "abs") == ".abs"
-- > sc_unaryMessage_pp (stParse unaryMessage "Required") == ".required"
sc_unaryMessage_pp :: UnaryMessage -> String
sc_unaryMessage_pp = ('.' :) . downcaseFirstLetter . selectorIdentifier . unaryMessageSelector

-- > sc_binaryMessage_pp (stParse binaryMessage "= 0") == "== 0"
-- > sc_binaryMessage_pp (stParse binaryMessage "+ 2") == "+ 2"
sc_binaryMessage_pp :: BinaryMessage -> String
sc_binaryMessage_pp (BinaryMessage b a) = strjn [sc_binop_rewrite b,sc_binaryArgument_pp a]

-- > sc_keywordMessage_pp (stParse keywordMessage "at: 0") == ".at(0)"
-- > sc_keywordMessage_pp (stParse keywordMessage "at: 0 put: x") == ".atPut(0,x)"
sc_keywordMessage_pp :: KeywordMessage -> String
sc_keywordMessage_pp (KeywordMessage l) =
  let (ks,as) = unzip l
  in printf ".%s(%s)" (sc_keywordSelector ks) (strjnComma (map sc_keywordArgument_pp as))

sc_binaryArgument_pp :: BinaryArgument -> String
sc_binaryArgument_pp (BinaryArgument p m) =
  strjn [sc_primary_pp p,maybe "" (strjn . map sc_unaryMessage_pp) m]

sc_keywordArgument_pp :: KeywordArgument -> String
sc_keywordArgument_pp (KeywordArgument p m1 m2) =
  strjn [sc_primary_pp p
        ,maybe "" (strjn . map sc_unaryMessage_pp) m1
        ,maybe "" (strjn . map sc_binaryMessage_pp) m2]

sc_cascadedMessages_pp :: CascadedMessages -> String
sc_cascadedMessages_pp = error "CascadedMessages?"

sc_literal_pp :: St.Literal -> String
sc_literal_pp lit =
  case lit of
    St.NumberLiteral n -> St.number_pp n
    St.StringLiteral s -> printf "\"%s\"" s
    St.CharacterLiteral c -> printf "$%c" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral s -> printf "\\%s" (St.selector_pp s)
    St.ArrayLiteral a -> printf "#[%s]" (St.strjnWith ',' (map (either sc_literal_pp id) a))

sc_number_pp :: Number -> String
sc_number_pp = numberEither show (\x -> Numeric.showFFloat Nothing x "")

sc_quotedCharacter_pp :: QuotedCharacter -> String
sc_quotedCharacter_pp = printf "$%c"

sc_quotedString_pp :: QuotedString -> String
sc_quotedString_pp = printf "\"%s\""

sc_hashedString_pp :: HashedString -> String
sc_hashedString_pp = printf "'%s'"

sc_selector_pp :: Selector -> String
sc_selector_pp sel =
  case sel of
    UnarySelector u -> u
    BinarySelector b -> b
    KeywordSelector k -> k
