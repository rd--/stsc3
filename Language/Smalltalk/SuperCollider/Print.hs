module Language.Smalltalk.SuperCollider.Print where

import Data.Char {- base -}
import Data.List {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import Language.Smalltalk.ANSI {- stsc3 -}

strjnComma :: [String] -> String
strjnComma = strjnWith ','

sc_smalltalkProgram_pp :: SmalltalkProgram -> String
sc_smalltalkProgram_pp = unlines . map sc_programElement_pp . programElements

sc_programElement_pp :: ProgramElement -> String
sc_programElement_pp el =
  case el of
    ProgramGlobal g -> sc_globalDefinition_pp g
    ProgramInitializer i -> sc_programInitializerDefinition_pp i

sc_ClassDefinition_pp :: ClassDefinition -> String
sc_ClassDefinition_pp c =
  let l f x = if null x then "" else f x
  in unlines
     [printf "%s %s {" (className c) (maybe "" (\x -> printf " : %s" x) (superclassName c))
     ,l (\x -> printf "var %s;" (strjnComma (map ("<>" ++) x))) (classInstanceVariableNames c)
     ,l (\x -> printf "classvar %s;" (strjnComma (map ("<>" ++) x))) (classVariableNames c)
     ,unlines (map (sc_methodDefinition_pp Nothing) (instanceMethods c))
     ,unlines (map (sc_methodDefinition_pp (Just '*')) (classMethods c))
     ,"}"]

sc_globalDefinition_pp :: GlobalDefinition -> String
sc_globalDefinition_pp (GlobalDefinition n i) = strjn [n,maybe "" sc_variableInitializer_pp i]

sc_variableInitializer_pp :: VariableInitializer -> String
sc_variableInitializer_pp = sc_initializerDefinition_pp

sc_programInitializerDefinition_pp :: ProgramInitializerDefinition -> String
sc_programInitializerDefinition_pp = sc_initializerDefinition_pp

sc_keywordSelector :: [Identifier] -> Identifier
sc_keywordSelector k =
  let remc = filter (/= ':')
      cap s = if length s > 1 then (toUpper (head s)) : tail s else s
  in case k of
       [] -> error "sc_keywordSelector?"
       [k0] -> remc k0
       k0:kN -> remc (k0 ++ concatMap cap kN)

{- | This rewrites the symbol "=" (to "==").
     This is required for translation.
     Other rewriting is defered.
-}
sc_binop_rewrite :: BinarySelector -> BinarySelector
sc_binop_rewrite b = if b == "=" then "==" else b

-- > sc_patternSelector (stParse messagePattern "= x") == "=="
sc_patternSelector :: Pattern -> Identifier
sc_patternSelector pat =
  case pat of
    UnaryPattern u -> u
    BinaryPattern b _ -> sc_binop_rewrite b
    KeywordPattern kp -> sc_keywordSelector (map fst kp)

sc_methodDefinition_pp :: Maybe Char -> MethodDefinition -> String
sc_methodDefinition_pp maybeStar (MethodDefinition p t s) =
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

sc_returnStatement_pp :: ReturnStatement -> String
sc_returnStatement_pp (ReturnStatement e) = printf "^%s" (sc_expression_pp e)

sc_expression_pp :: Expression -> String
sc_expression_pp = expressionEither sc_assignment_pp sc_basicExpression_pp

sc_assignment_pp :: Assignment -> String
sc_assignment_pp (Assignment i e) = printf "%s = %s" i (sc_expression_pp e)

areBinaryMessages :: Maybe Messages -> Bool
areBinaryMessages m =
  case m of
    Just (MessagesBinary _ _) -> True
    _ -> False

{- | Binary messages must be parenthesised because keyword messages have higher precedence.

> let p = sc_basicExpression_pp . stParse basicExpression
> p "x * y + z min: 3" == "( x * y + z ) .min(3)"
> p "x == 0 ifTrue: [y]" == "( x == 0 ) .ifTrue({y})"
-}
sc_basicExpression_pp :: BasicExpression -> String
sc_basicExpression_pp (BasicExpression p m c) =
  strjn [if areBinaryMessages m then "(" else ""
        ,sc_primary_pp p
        ,maybe "" sc_messages_pp m
        ,maybe "" sc_cascadedMessages_pp c]

{- | In SuperCollider keyword patterns bind more closely than binary patterns.

> let p = sc_messages_pp . stParse messages
> p "min: 2 * 3"
> p "at: 0 put: x * 2"
> p "== 0 ifTrue: [error]" -- the initial binary operator needs to be parenthesised....
-}
sc_messages_pp :: Messages -> String
sc_messages_pp ms =
  case ms of
    MessagesUnary m1 m2 m3 -> strjn (concat ([map sc_unaryMessage_pp m1
                                             ,maybe [] (map sc_binaryMessage_pp) m2
                                             ,maybe [] (return . sc_keywordMessage_pp) m3]))
    MessagesBinary m1 m2 -> strjn (concat ([map sc_binaryMessage_pp m1
                                           ,[")"] -- close binary messages ; precedence
                                           ,maybe [] (return . sc_keywordMessage_pp) m2]))
    MessagesKeyword m1 -> sc_keywordMessage_pp m1

{- | This rewrites the identifiers "this" (to "self").
     This is required for translation.
     Other rewriting is defered.

> sc_primary_pp (stParse primary "this") == "self"
-}
sc_primary_pp :: Primary -> String
sc_primary_pp pr =
  case pr of
    PrimaryIdentifier i -> case i of
                             "this" -> "self"
                             _ -> i
    PrimaryLiteral l -> sc_literal_pp l
    PrimaryBlock b -> sc_blockBody_pp b
    PrimaryExpression e -> printf "(%s)" (sc_expression_pp e)
    PrimaryArrayExpression a -> printf "{%s}" (intercalate " . " (map sc_basicExpression_pp a))

-- > sc_unaryMessage_pp (stParse unaryMessage "abs") == ".abs"
sc_unaryMessage_pp :: UnaryMessage -> String
sc_unaryMessage_pp = ('.' :) . unaryMessageSelector

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

sc_literal_pp :: Literal -> String
sc_literal_pp lit =
  case lit of
    NumberLiteral n -> number_pp n
    StringLiteral s -> printf "\"%s\"" s
    CharacterLiteral c -> printf "$%c" c
    SymbolLiteral s -> printf "'%s'" s
    SelectorLiteral s -> printf "\\%s" (selector_pp s)
    ArrayLiteral a -> printf "#[%s]" (strjnComma (map (either sc_literal_pp id) a))

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
