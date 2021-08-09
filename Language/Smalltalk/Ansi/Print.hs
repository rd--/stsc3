-- | Pretty printer for a subset of ANSI Smalltalk.
module Language.Smalltalk.Ansi.Print where

import Data.List {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import Language.Smalltalk.Ansi {- stsc3 -}

-- | 'intercalate', removing empty input strings.  Used for pretty printing.
strjnWith :: Char -> [String] -> String
strjnWith x = intercalate [x] . filter (not . null)

-- | 'strjnWith' space.
strjn :: [String] -> String
strjn = strjnWith ' '

{- | Read file, parse as 'smalltalkProgram', pretty print and write to file.

> stParse smalltalkProgram "Transcript show: 'text'"
> stParse smalltalkProgram "1 to: 5 do: [:x| Transcript cr ; show: x]"

> st <- readFile "/home/rohan/sw/stsc3/help/graph/jmcc-strummable-silk.st"
> putStrLn $ smalltalkProgram_pp $ stParse smalltalkProgram st
-}
stRewrite :: FilePath -> FilePath -> IO ()
stRewrite st_fn rw_fn = do
  st <- readFile st_fn
  let p = stParse smalltalkProgram st
  writeFile rw_fn (smalltalkProgram_pp p)

-- > smalltalkProgram_pp (SmalltalkProgram []) == ""
smalltalkProgram_pp :: SmalltalkProgram -> String
smalltalkProgram_pp = unlines . map programElement_pp . programElements

programElement_pp :: ProgramElement -> String
programElement_pp el =
  case el of
    ProgramGlobal g -> globalDefinition_pp g
    ProgramInitializer i -> programInitializerDefinition_pp i

globalDefinition_pp :: GlobalDefinition -> String
globalDefinition_pp (GlobalDefinition n i) = strjn [n,maybe "" variableInitializer_pp i,"\n"]

variableInitializer_pp :: VariableInitializer -> String
variableInitializer_pp = initializerDefinition_pp

programInitializerDefinition_pp :: ProgramInitializerDefinition -> String
programInitializerDefinition_pp = initializerDefinition_pp

{-
> methodDefinition_pp $ stParse methodDefinition "midicps ^ 440 * (2 ** ((self - 69) * (1 / 12)))"
-}
methodDefinition_pp :: MethodDefinition -> String
methodDefinition_pp (MethodDefinition _ p t s) = strjn [pattern_pp p,maybe "" temporaries_pp t,maybe "" statements_pp s]

{-
> pattern_pp (stParse messagePattern "midicps")
> pattern_pp (stParse messagePattern "+ aNumber")
> pattern_pp (stParse messagePattern "freq: f phase: p")
-}
pattern_pp :: Pattern -> String
pattern_pp pat =
  case pat of
    UnaryPattern u -> u
    BinaryPattern b a -> strjn [b,a]
    KeywordPattern kp -> strjn (concatMap (\(k,a) -> [k,a]) kp)

temporaries_pp :: Temporaries -> String
temporaries_pp (Temporaries t) = printf "|%s|\n" (strjn t)

initializerDefinition_pp :: InitializerDefinition -> String
initializerDefinition_pp (InitializerDefinition t s) =
  strjn [maybe "" temporaries_pp t,maybe "" statements_pp s]

blockBody_pp :: BlockBody -> String
blockBody_pp (BlockBody a t s) =
  strjn ["["
        ,maybe "" (printf "%s|" . strjn . map blockArgument_pp) a
        ,maybe "" temporaries_pp t
        ,maybe "" statements_pp s
        ,"]"]

blockArgument_pp :: BlockArgument -> String
blockArgument_pp = (:) ':'

statements_pp :: Statements -> String
statements_pp st =
  case st of
    StatementsReturn r -> returnStatement_pp r
    StatementsExpression e st' -> strjn [expression_pp e,".\n",maybe "" statements_pp st']

returnStatement_pp :: ReturnStatement -> String
returnStatement_pp (ReturnStatement e) = printf "^%s" (expression_pp e)

expression_pp :: Expression -> String
expression_pp = expressionEither assignment_pp basicExpression_pp

assignment_pp :: Assignment -> String
assignment_pp (Assignment i e) = printf "%s := %s" i (expression_pp e)

basicExpression_pp :: BasicExpression -> String
basicExpression_pp (BasicExpression p m c) =
  strjn [primary_pp p
        ,maybe "" messages_pp m
        ,maybe "" cascadedMessages_pp c]

messages_pp :: Messages -> String
messages_pp ms =
  case ms of
    MessagesUnary m1 m2 m3 -> strjn (concat ([map unaryMessage_pp m1
                                             ,maybe [] (map binaryMessage_pp) m2
                                             ,maybe [] (return . keywordMessage_pp) m3]))
    MessagesBinary m1 m2 -> strjn (concat ([map binaryMessage_pp m1
                                           ,maybe [] (return . keywordMessage_pp) m2]))
    MessagesKeyword m1 -> keywordMessage_pp m1

primary_pp :: Primary -> String
primary_pp pr =
  case pr of
    PrimaryIdentifier i -> i
    PrimaryLiteral l -> literal_pp l
    PrimaryBlock b -> blockBody_pp b
    PrimaryExpression e -> printf "(%s)" (expression_pp e)
    PrimaryArrayExpression a -> printf "{%s}" (intercalate " . " (map basicExpression_pp a))

unaryMessage_pp :: UnaryMessage -> String
unaryMessage_pp = selectorIdentifier . unaryMessageSelector

binaryMessage_pp :: BinaryMessage -> String
binaryMessage_pp (BinaryMessage b a) = strjn [b,binaryArgument_pp a]

keywordMessage_pp :: KeywordMessage -> String
keywordMessage_pp (KeywordMessage l) =
  let f (k,a) = strjn [k,keywordArgument_pp a]
  in strjn (map f l)

binaryArgument_pp :: BinaryArgument -> String
binaryArgument_pp (BinaryArgument p m) = strjn [primary_pp p,maybe "" (strjn . map unaryMessage_pp) m]

keywordArgument_pp :: KeywordArgument -> String
keywordArgument_pp (KeywordArgument p m1 m2) =
  strjn [primary_pp p
        ,maybe "" (strjn . map unaryMessage_pp) m1
        ,maybe "" (strjn . map binaryMessage_pp) m2]

cascadedMessages_pp :: CascadedMessages -> String
cascadedMessages_pp = strjn . map ((++) "; " . messages_pp)

-- > map literal_pp [SelectorLiteral (UnarySelector "dinf")]
literal_pp :: Literal -> String
literal_pp lit =
  case lit of
    NumberLiteral n -> number_pp n
    StringLiteral s -> printf "'%s'" s
    CharacterLiteral c -> printf "$%c" c
    SymbolLiteral s -> printf "#'%s'" s
    SelectorLiteral s -> printf "#%s" (selector_pp s)
    ArrayLiteral a -> printf "#(%s)" (strjn (map (either literal_pp id) a))

number_pp :: Number -> String
number_pp = numberEither show (\n -> Numeric.showFFloat Nothing n "")

quotedCharacter_pp :: QuotedCharacter -> String
quotedCharacter_pp = printf "$%c"

quotedString_pp :: QuotedString -> String
quotedString_pp = printf "'%s'"

hashedString_pp :: HashedString -> String
hashedString_pp = printf "#'%s'"

selector_pp :: Selector -> String
selector_pp sel =
  case sel of
    UnarySelector u -> u
    BinarySelector b -> b
    KeywordSelector k -> k
