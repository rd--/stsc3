-- | Parser and pretty printer for a subset of ANSI Smalltalk.
module Language.Smalltalk.Parser where

import Data.Functor.Identity {- base -}
import Data.List {- base -}
import qualified Numeric {- base -}
import Text.Printf {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.Language as P {- parsec -}
import qualified Text.Parsec.String as P {- parsec -}
import qualified Text.Parsec.Token as P {- parsec -}

-- | A 'Char' parser with no user state.
type P a = P.GenParser Char () a

-- | Run p then q, returning result of p.
(>>~) :: Monad m => m t -> m u -> m t
p >>~ q = p >>= \x -> q >> return x

-- | /p/ consuming any trailing separators.
lexeme :: P t -> P t
lexeme = P.lexeme stLexer

-- | Smalltalk language definition (for token parser)
stLanguageDef :: P.GenLanguageDef String () Identity
stLanguageDef =
  P.LanguageDef
  {P.commentStart = "\""
  ,P.commentEnd = "\""
  ,P.commentLine = "" -- NIL
  ,P.nestedComments = False
  ,P.identStart = letter
  ,P.identLetter = letter P.<|> P.digit
  ,P.opStart = binaryCharacter
  ,P.opLetter = binaryCharacter
  ,P.reservedNames = stReservedIdentifiers
  ,P.reservedOpNames = []
  ,P.caseSensitive = True}

-- | Lexer
stLexer :: P.GenTokenParser String () Identity
stLexer = P.makeTokenParser stLanguageDef

-- | Run parser and report any error.
stParse :: P t -> String -> t
stParse p = either (\m -> error ("stParse: " ++ show m)) id . P.parse p ""

-- | 'unwords', removing empty input strings.
strjn :: [String] -> String
strjn = unwords . filter (not . null)

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

-- * 3.3.1 Program Definition

type SmalltalkProgram = [ProgramElement]

smalltalkProgram_pp :: SmalltalkProgram -> String
smalltalkProgram_pp = unlines . map programElement_pp

-- | <<Smalltalk program>> ::= <<program element>>+ <<initialization ordering>>
smalltalkProgram :: P SmalltalkProgram
smalltalkProgram = P.optional separator >> P.many1 programElement

data ProgramElement
  = ProgramGlobal GlobalDefinition
  | ProgramInitializer ProgramInitializerDefinition
  deriving (Eq,Show)

programElement_pp :: ProgramElement -> String
programElement_pp el =
  case el of
    ProgramGlobal g -> globalDefinition_pp g
    ProgramInitializer i -> programInitializerDefinition_pp i

-- | <<program element>> ::= <<class definition>> | <<global definition>> | <<pool definition>> | <<program initializer definition>>
programElement :: P ProgramElement
programElement = P.choice [fmap ProgramInitializer programInitializerDefinition
                          ,fmap ProgramGlobal globalDefinition]

-- * 3.3.3 Global Variable Definition

type GlobalDefinition = (GlobalName,Maybe VariableInitializer)

globalDefinition_pp :: GlobalDefinition -> String
globalDefinition_pp (n,i) = strjn [n,maybe "" variableInitializer_pp i,"\n"]

-- | <<global definition>> ::= [<<constant designator>>] <<global name>> [<<variable initializer>>]
globalDefinition :: P GlobalDefinition
globalDefinition = do
  -- constant designator
  n <- identifier
  v <- P.optionMaybe initializerDefinition
  return (n,v)

-- | <<global name>> ::= identifier
type GlobalName = Identifier

-- | <<variable initializer>> ::= <initializer definition>
type VariableInitializer = InitializerDefinition

variableInitializer_pp :: VariableInitializer -> String
variableInitializer_pp = initializerDefinition_pp

-- * 3.3.5 Program Initializer Definition

type ProgramInitializerDefinition = InitializerDefinition

{- | <<program initializer definition >> ::= <initializer definition>

> stParse programInitializerDefinition "\"x\" |t| t + 1"
> stParse programInitializerDefinition "" -- FAIL
-}
programInitializerDefinition :: P ProgramInitializerDefinition
programInitializerDefinition = initializerDefinition

programInitializerDefinition_pp :: ProgramInitializerDefinition -> String
programInitializerDefinition_pp = initializerDefinition_pp

-- * 3.4.2

data MethodDefinition = MethodDefinition Pattern (Maybe Temporaries) (Maybe Statements) deriving (Eq,Show)

methodDefinition_pp :: MethodDefinition -> String
methodDefinition_pp (MethodDefinition p t s) = strjn [pattern_pp p,maybe "" temporaries_pp t,maybe "" statements_pp s]

{- | <method definition> ::= <message pattern> [<temporaries>] [<statements>]

> stParse methodDefinition "p"
> stParse methodDefinition "p |t|"
> stParse methodDefinition "p |t| ^s"
> stParse methodDefinition "p ^s"
> methodDefinition_pp $ stParse methodDefinition "midicps ^ 440 * (2 ** ((self - 69) * (1 / 12)))"
-}
methodDefinition :: P MethodDefinition
methodDefinition = do
  p <- messagePattern
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  return (MethodDefinition p t s)

data Pattern
  = UnaryPattern Identifier
  | BinaryPattern (Identifier,Identifier)
  | KeywordPattern [(Keyword,Identifier)]
  deriving (Eq, Show)

pattern_pp :: Pattern -> String
pattern_pp pat =
  case pat of
    UnaryPattern u -> u
    BinaryPattern (b,a) -> strjn [b,a]
    KeywordPattern kp -> strjn (concatMap (\(k,a) -> [k,a]) kp)

{- | <message pattern> ::= <unary pattern> | <binary pattern> | <keyword pattern>

> stParse messagePattern "p" == UnaryPattern "p"
> stParse messagePattern "+p" == stParse messagePattern "+ p"
> stParse messagePattern "k1:p1 k2:p2" == stParse messagePattern "k1: p1 k2: p2"
-}
messagePattern :: P Pattern
messagePattern = P.choice [(P.try keywordPattern),(P.try binaryPattern),unaryPattern] P.<?> "messagePattern"

{- | <unary pattern> ::= unarySelector

> stParse unaryPattern "p"
> stParse unaryPattern "p:" -- FAIL
-}
unaryPattern :: P Pattern
unaryPattern = fmap UnaryPattern unarySelector -- lexeme

{- | <binary pattern> ::= binarySelector <method argument>

> stParse binaryPattern "+p" == stParse binaryPattern "+ p"
-}
binaryPattern :: P Pattern
binaryPattern = do
  sel <- binarySelector
  arg <- identifier
  return (BinaryPattern (sel,arg))

{- | <keyword pattern> ::= (keyword <method argument>)+

> stParse keywordPattern "k: p"
> stParse keywordPattern "k1: p1 k2: p2"
> stParse keywordPattern "k1: p1 k2:" -- FAIL
> stParse keywordPattern "k1: p1 p2" -- FAIL
-}
keywordPattern :: P Pattern
keywordPattern = do
  let f = do kw <- keyword
             arg <- identifier
             return (kw,arg)
  fmap KeywordPattern (P.many1 f)

type Temporaries = [Identifier]

temporaries_pp :: Temporaries -> String
temporaries_pp = printf "|%s|\n" . strjn

verticalBar :: P Char
verticalBar = lexeme (P.char '|')

{- | <temporaries> ::= '|' <temporary variable list> '|'

> stParse temporaries "||"
> stParse temporaries "|p|"
> stParse temporaries "| p|"
> stParse temporaries "|p |"
> stParse temporaries "|p q r|"
-}
temporaries :: P Temporaries
temporaries = P.label (P.between verticalBar verticalBar temporary_variable_list) "temporaries"

{- | <temporary variable list> ::= identifier*

> stParse temporary_variable_list ""
> stParse temporary_variable_list "p"
> stParse temporary_variable_list "p q"
> stParse temporary_variable_list "p q r"
> stParse temporary_variable_list "p q r +"
-}
temporary_variable_list :: P [Identifier]
temporary_variable_list = P.many identifier P.<?> "temporary_variable_list"

-- * 3.4.3

data InitializerDefinition = InitializerDefinition (Maybe Temporaries) (Maybe Statements) deriving (Eq,Show)

initializerDefinition_pp :: InitializerDefinition -> String
initializerDefinition_pp (InitializerDefinition t s) = strjn [maybe "" temporaries_pp t,maybe "" statements_pp s]

{- | <initializer definition> ::= [<temporaries>] [<statements>]

> stParse initializerDefinition "|a b| a := 1 . ^ a + b ."
> stParse initializerDefinition "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> stParse initializerDefinition "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> stParse initializerDefinition "|a b c| a := 1 . b := 2 . c := 3 . ^ a + b + c ."
> stParse initializerDefinition "|a b c| a := [1] . b := [2] . c := [3] . ^ a value + b value + c value ."
> stParse initializerDefinition "" -- FAIL
-}
initializerDefinition :: P InitializerDefinition
initializerDefinition = do
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  case (t,s) of
    (Nothing,Nothing) -> P.unexpected "initializerDefinition: empty"
    _ -> return (InitializerDefinition t s)

-- * 3.4.4

{- | <block constructor> ::= '[' <block body> ']'

> stParse blockConstructor "[1]" == stParse blockConstructor "[ 1 ]"
> stParse blockConstructor "[^1]" ==  stParse blockConstructor "[ ^ 1 .]"
> stParse blockConstructor "[:a| |t|]" == stParse blockConstructor "[ :a | | t | ]"
> stParse blockConstructor "[:a| |t u| x . y]"
> stParse blockConstructor "[:a| |t u| t := [:b| |v w| z] . x . y]"
-}
blockConstructor :: P BlockBody
blockConstructor = inBrackets blockBody

data BlockBody = BlockBody (Maybe [BlockArgument]) (Maybe Temporaries) (Maybe Statements) deriving (Eq,Show)

blockBody_pp :: BlockBody -> String
blockBody_pp (BlockBody a t s) =
  strjn ["["
        ,maybe "" (printf "%s|" . strjn . map blockArgument_pp) a
        ,maybe "" temporaries_pp t
        ,maybe "" statements_pp s
        ,"]"]

{- | <block body> ::= [<block argument>* '|'] [<temporaries>] [<statements>]

> stParse blockBody ""
> stParse blockBody ":a|"
> stParse blockBody ":a :b|"
> stParse blockBody ":a| |p q r|"
> stParse blockBody ":a :b| |p q r|"
> stParse blockBody "|p q r|"
> stParse blockBody "1"
> stParse blockBody ":a| a + 1"
> stParse blockBody ":a| |b| b := a + 1. ^b"
-}
blockBody :: P BlockBody
blockBody = do
  a <- P.optionMaybe (P.try (P.many1 blockArgument >>~ verticalBar))
  P.optional separator
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  return (BlockBody a t s)

type BlockArgument = Identifier

blockArgument_pp :: BlockArgument -> String
blockArgument_pp = (:) ':'

{- | <block argument> ::= ':' identifier

> stParse blockArgument ":a"
-}
blockArgument :: P BlockArgument
blockArgument = P.char ':' >> identifier

-- * 3.4.5

data Statements
  = StatementsReturn ReturnStatement
  | StatementsExpression Expression (Maybe Statements)
  deriving (Eq,Show)

statements_pp :: Statements -> String
statements_pp st =
  case st of
    StatementsReturn r -> returnStatement_pp r
    StatementsExpression e st' -> strjn [expression_pp e,".\n",maybe "" statements_pp st']

period :: P String
period = P.dot stLexer

{- | <statements> ::= (<return statement> ['.']) | (<expression> ['.' [<statements>]])

> stParse statements "^p" == stParse statements "^ p"
> stParse statements "^p." == stParse statements "^p ."
> stParse statements "^ p." == stParse statements "^ p ."
> stParse statements "p"
> stParse statements "p q"
> stParse statements "(p q)"
> stParse statements "p.^q" == stParse statements "p .^q"
> stParse statements "p . ^q" ==  stParse statements "p . ^ q"
> stParse statements "^ 440 * (2 ** ((self - 69) * (1 / 12)))"
-}
statements :: P Statements
statements = do
  let rhs = do
        e <- expression
        s <- P.optionMaybe (period >> statements)
        return (StatementsExpression e s)
  P.choice [fmap StatementsReturn (returnStatement >>~ P.optional period),rhs]

data ReturnStatement = ReturnStatement Expression deriving (Eq,Show)

returnStatement_pp :: ReturnStatement -> String
returnStatement_pp (ReturnStatement e) = printf "^%s" (expression_pp e)

{- | <return statement> ::= returnOperator <expression>

> stParse returnStatement "^1"
> stParse returnStatement "^p"
> stParse returnStatement "^ 1"
> stParse returnStatement "^ p"
> stParse returnStatement "^ a value + b value + c value"
-}
returnStatement :: P ReturnStatement
returnStatement = fmap ReturnStatement (returnOperator >> expression)

type Expression = Either Assignment BasicExpression

expression_pp :: Expression -> String
expression_pp = either assignment_pp basicExpression_pp

{- | <expression> ::= <assignment> | <basic expression>

> stParse expression "p := 1"
> stParse expression "p := q"
> stParse expression "p"
> stParse expression "1"
> stParse expression "1 + 2"
> stParse expression "p := (1 + 2)"
> stParse expression "p := (1 + 2) negate"
> stParse expression "a value + b value + c value"
> stParse expression "note2 := (sequR value: #(-12 -7 -5 0 2 5) value: clock_16) + note1"
> stParse expression "freq := ((#(-2 0 3 5 7 10 12 15) at: i) + 60) midicps ."
> stParse expression "(1 to: 6) do: [:i| s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand . 0.05 rand} decaytime: 4] . \"rvb\""
> stParse expression "out := pitch size mixFill: [:i| |trigger pluck period string| trigger := HPZ1 in: (mousex > (0.25 + ((i - 1) * triggerSpacing))) abs . x]"
> stParse expression "out := pitch size mixFill: [:i| x]"
> stParse expression "pitch size mixFill: x"
> stParse expression "[1] value"
> stParse expression "o m: true"
-}
expression :: P Expression
expression = fmap Left (P.try assignment) P.<|> fmap Right basicExpression

data Assignment = Assignment Identifier Expression deriving (Eq,Show)

assignment_pp :: Assignment -> String
assignment_pp (Assignment i e) = printf "%s := %s" i (expression_pp e)

{- | <assignment> ::= <assignment target> assignmentOperator <expression>

> stParse assignment "p:=1"
> stParse assignment "p :=1"
> stParse assignment "p:= 1"
> stParse assignment "p := 1"
> stParse assignment "p := q"
> stParse assignment "p := 2.0"
> stParse assignment "p := 'x'"
> stParse assignment "p := 8 mixFill: [:i| |n trigger pluck freq z metal| n := 15 ]"
-}
assignment :: P Assignment
assignment = do
  a <- assignmentTarget
  _ <- assignmentOperator
  e <- expression
  return (Assignment a e)

type BasicExpression = (Primary,Maybe Messages,Maybe CascadedMessages)

basicExpression_pp :: BasicExpression -> String
basicExpression_pp (p,m,c) =
  strjn [primary_pp p
        ,maybe "" messages_pp m
        ,maybe "" cascadedMessages_pp c]

{- | <basic expression> ::= <primary> [<messages> <cascaded messages>]

> stParse basicExpression "1"
> stParse basicExpression "p"
> stParse basicExpression "1 negate"
> stParse basicExpression "1 + 2"
> stParse basicExpression "(p)"
> stParse basicExpression "(p 1)"
> stParse basicExpression "a value + b value + c value"
> stParse basicExpression "p q r: x"
-}
basicExpression :: P BasicExpression
basicExpression = do
  p <- primary
  m <- P.optionMaybe messages
  c <- P.optionMaybe cascadedMessages
  return (p,m,c)

-- | <assignment target> := identifier
assignmentTarget :: P Identifier
assignmentTarget = P.label identifier "assignmentTarget"

data Messages
  = UnaryMessages ([Message],Maybe [Message],Maybe Message)
  | BinaryMessages ([Message],Maybe Message)
  | KeywordMessages Message
  deriving (Eq,Show)

messages_pp :: Messages -> String
messages_pp ms =
  case ms of
    UnaryMessages (m1,m2,m3) -> strjn (concat ([map message_pp m1,maybe [] (map message_pp) m2,maybe [] (return . message_pp) m3]))
    BinaryMessages (m1,m2) -> strjn (concat ([map message_pp m1,maybe [] (return . message_pp) m2]))
    KeywordMessages m1 -> message_pp m1

data Primary
  = PrimaryIdentifier Identifier
  | PrimaryLiteral Literal
  | PrimaryBlock BlockBody
  | PrimaryExpression Expression
  | PrimaryArrayExpression [BasicExpression] -- NON-ANSI
  deriving (Eq, Show)

primary_pp :: Primary -> String
primary_pp pr =
  case pr of
    PrimaryIdentifier i -> i
    PrimaryLiteral l -> literal_pp l
    PrimaryBlock b -> blockBody_pp b
    PrimaryExpression e -> printf "(%s)" (expression_pp e)
    PrimaryArrayExpression a -> printf "{%s}" (intercalate " . " (map basicExpression_pp a))

inParentheses :: P t -> P t
inParentheses = P.parens stLexer

inBraces :: P t -> P t
inBraces = P.braces stLexer

inBrackets :: P t -> P t
inBrackets = P.brackets stLexer

{- | <primary> ::= identifier | <literal> | <block constructor> | ( '(' <expression> ')' )

> stParse primary "p" == PrimaryIdentifier "p"
> stParse primary "1"
> primary_pp (stParse primary "[:a| a + 1]")
> primary_pp (stParse primary "(p)")
> primary_pp (stParse primary "(p q)") == "(p q)"
> primary_pp (stParse primary "(p q: r)")
> primary_pp (stParse primary "(1 + 2)")
> primary_pp (stParse primary "{p.q.r}")
-}
primary :: P Primary
primary = P.choice [fmap PrimaryBlock blockConstructor
                   ,fmap PrimaryExpression (inParentheses expression)
                   ,fmap PrimaryArrayExpression (inBraces (P.sepBy basicExpression period))
                   ,fmap PrimaryIdentifier identifier
                   ,fmap PrimaryLiteral literal] P.<?> "primary"

{- | <unary message>+ <binary message>* [<keyword message>]

> stParse unaryMessages "abs"
> stParse unaryMessages "abs square"
> stParse unaryMessages "abs square +p" == stParse unaryMessages "abs square + p"
> stParse unaryMessages "abs square +p +q" == stParse unaryMessages "abs square + p + q"
> stParse unaryMessages "value +b value +c value" == stParse unaryMessages "value + b value + c value"
> stParse unaryMessages "abs square +p +q k:r" == stParse unaryMessages "abs square + p + q k: r"
> stParse unaryMessages "abs square +p +q k1:p1 k2:p2" == stParse unaryMessages "abs square + p + q k1: p1 k2: p2"
> stParse unaryMessages "q r:x" == stParse unaryMessages "q r: x"
-}
unaryMessages :: P Messages
unaryMessages = do
  u <- P.many1 (P.try unaryMessage)
  b <- P.optionMaybe (P.many binaryMessage)
  P.optional separator
  k <- P.optionMaybe keywordMessage
  return (UnaryMessages (u,b,k))

-- | <binary message>+ [<keyword message>]
binaryMessages :: P Messages
binaryMessages = do
  b <- P.many1 binaryMessage
  k <- P.optionMaybe keywordMessage
  return (BinaryMessages (b,k))

{- | <messages> ::= (<unary message>+ <binary message>* [<keyword message>] ) | (<binary message>+ [<keyword message>] ) | <keyword message>

> rw = messages_pp . stParse messages
> (rw "k1:p1",rw "k1:p1 k2:p2")
> (rw "+p",rw "+p +q")
> (rw "+p k:r",rw "+p +q k:r")

> stParse messages "k1:p1" == stParse messages "k1: p1"
> stParse messages "k1: p1 k2: p2" == stParse messages "k1:p1 k2:p2"
> stParse messages "+1" == stParse messages "+ 1"
> stParse messages "+p" == stParse messages "+ p"
> stParse messages "+p +q" == stParse messages "+ p + q"
> stParse messages "+p +q k:r" == stParse messages "+ p + q k: r"
> stParse messages "q r: x"
-}
messages :: P Messages
messages = P.choice [P.try binaryMessages,P.try (fmap KeywordMessages keywordMessage),unaryMessages]

data Message
  = UnaryMessage Identifier
  | BinaryMessage (Identifier,BinaryArgument)
  | KeywordMessage [(Identifier,KeywordArgument)]
  deriving (Eq,Show)

message_pp :: Message -> String
message_pp msg =
  let keywordMessage_pp :: (Identifier,KeywordArgument) -> String
      keywordMessage_pp (k,a) = strjn [k,keywordArgument_pp a]
  in case msg of
       UnaryMessage u -> u
       BinaryMessage (b,a) -> strjn [b,binaryArgument_pp a]
       KeywordMessage k -> strjn (map keywordMessage_pp k)

{- | <unary message> ::= unarySelector

> stParse unaryMessage "p" == UnaryMessage "p"
> stParse unaryMessage "" -- FAIL
-}
unaryMessage :: P Message
unaryMessage = fmap UnaryMessage unarySelector -- lexeme

{- | <binary message> ::= binarySelector <binary argument>

> stParse binaryMessage "+p" == stParse binaryMessage "+ p"
> stParse binaryMessage "+1" == stParse binaryMessage "+ 1"
> stParse binaryMessage "+1.0" == stParse binaryMessage "+ 1.0"
> stParse binaryMessage "+ 1 k:" -- + 1
-}
binaryMessage :: P Message
binaryMessage = do
  sel <- binarySelector
  arg <- binaryArgument
  return (BinaryMessage (sel,arg))

type BinaryArgument = (Primary,Maybe [Message])

binaryArgument_pp :: BinaryArgument -> String
binaryArgument_pp (p,m) = strjn [primary_pp p,maybe "" (strjn . map message_pp) m]

{- | <binary argument> ::= <primary> <unary message>*

> stParse binaryArgument "p"
> stParse binaryArgument "p q"
> stParse binaryArgument "p q r"
> stParse binaryArgument "p q r s:" -- p q r
-}
binaryArgument :: P BinaryArgument
binaryArgument = do
  p <- primary
  u <- P.optionMaybe (P.many (P.try unaryMessage))
  return (p,u)

{- | <keyword message> ::= (keyword <keyword argument>)+

> stParse keywordMessage "k:p" == stParse keywordMessage "k: p"
> stParse keywordMessage "k1:p1 k2:p2" == stParse keywordMessage "k1: p1 k2: p2"
-}
keywordMessage :: P Message
keywordMessage = do
  let f = do kw <- keyword
             arg <- keywordArgument
             return (kw,arg)
  fmap KeywordMessage (P.many1 f)

type KeywordArgument = (Primary,Maybe [Message],Maybe [Message])

keywordArgument_pp :: KeywordArgument -> String
keywordArgument_pp (p,m1,m2) = strjn [primary_pp p,maybe "" (strjn . map message_pp) m1,maybe "" (strjn . map message_pp) m2]

{- | <keyword argument> ::= <primary> <unary message>* <binary message>*

> stParse keywordArgument "p"
> stParse keywordArgument "1"
> stParse keywordArgument "p q"
> stParse keywordArgument "p q r"
> stParse keywordArgument "p q r + s"
> stParse keywordArgument "p q r + s * t"
> stParse keywordArgument "p k:" -- p
-}
keywordArgument :: P KeywordArgument
keywordArgument = do
  p <- primary
  u <- P.optionMaybe (P.many1 (P.try unaryMessage))
  b <- P.optionMaybe (P.many1 (P.try binaryMessage))
  return (p,u,b)

cascadeSeparator :: P String
cascadeSeparator = P.semi stLexer

type CascadedMessages = [Messages]

cascadedMessages_pp :: CascadedMessages -> String
cascadedMessages_pp = strjn . map ((++) "; " . messages_pp)

-- | <cascaded messages> ::= (';' <messages>)*
cascadedMessages :: P CascadedMessages
cascadedMessages = P.many (cascadeSeparator >> messages)

-- * 3.4.6

type Symbol = String

-- | <literal> ::= <number literal> | <string literal> | <character literal> | <symbol literal> | <selector literal> | <array literal>
data Literal
  = NumberLiteral Number
  | StringLiteral String
  | CharacterLiteral Char
  | SymbolLiteral Symbol
  | SelectorLiteral Selector
  | ArrayLiteral [Either Literal Identifier]
  deriving (Eq, Show)

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

{- | <literal> ::= <number literal> | <string literal> | <character literal> | <symbol literal> | <selector literal> | <array literal>

> stParse literal "123"
> stParse literal "-123"
> stParse literal "123.456"
> stParse literal "-123.456"
> stParse literal "'x'" == CharacterLiteral 'x'
> stParse literal "$x" == StringLiteral "x"
> stParse literal "#'xyz'" == SymbolLiteral "xyz"
> stParse literal "#abs" == SelectorLiteral (UnarySelector "abs")
> stParse literal "#freq:iphase:" == SelectorLiteral (KeywordSelector "freq:iphase:")
> stParse literal "#+" == SelectorLiteral (BinarySelector "+")
> stParse literal "#(1 2.0 'x' $x #'xyz' #abs #freq:iphase: #+)"
> stParse literal "#(-12 -7 -5 0 2 5)"
-}
literal :: P Literal
literal = P.choice [numberLiteral, stringLiteral, characterLiteral, P.try arrayLiteral, P.try symbolLiteral, selectorLiteral] -- lexeme

{- | <number literal> ::= ['-'] <number>

> stParse numberLiteral "123"
> stParse numberLiteral "123.456"
> stParse numberLiteral "-123"
> stParse numberLiteral "-123.456"
-}
numberLiteral :: P Literal
numberLiteral = do
  n <- P.optionMaybe (P.char '-')
  let rw :: Num n => n -> n
      rw = maybe id (const negate) n
  fmap (NumberLiteral . either (Left . rw) (Right . rw)) number -- lexeme

type Number = Either Integer Double

number_pp :: Number -> String
number_pp = either show (\n -> Numeric.showFFloat Nothing n "")

{- | <number> ::= integer | float | scaledDecimal

> map (stParse number) (words "1 1.2")
> map (stParse number) (words "-1 -1.2") -- FAIL
-}
number :: P Number
number = lexeme (fmap Right (P.try float) P.<|> fmap Left integer)

-- | <character literal> ::= quotedCharacter
characterLiteral :: P Literal
characterLiteral = fmap CharacterLiteral quotedCharacter

{- | <string literal> ::= quotedString

> stParse stringLiteral "'x'" == "x"
-}
stringLiteral :: P Literal
stringLiteral = fmap StringLiteral quotedString

-- | <symbol literal> ::= hashedString
symbolLiteral :: P Literal
symbolLiteral = fmap SymbolLiteral hashedString

-- | <selector literal> ::= quotedSelector
selectorLiteral :: P Literal
selectorLiteral = fmap SelectorLiteral quotedSelector

{- | <array literal> ::= '#(' <array element>* ')'

> stParse arrayLiteral "#()" == stParse arrayLiteral "#( )"
> stParse arrayLiteral "#(1)" == stParse arrayLiteral "#( 1 )"
> stParse arrayLiteral "#(1 2.0)"
> stParse arrayLiteral "#(1 2.0 3)"
> stParse arrayLiteral "#(1 2.0 p)"
-}
arrayLiteral :: P Literal
arrayLiteral = fmap ArrayLiteral (P.between (lexeme (P.string "#(")) (lexeme (P.char ')')) (P.many arrayElement))

{- | <array element> ::= <literal> | identifier

> stParse arrayElement "1"
> stParse arrayElement "2.0"
> stParse arrayElement "p"
-}
arrayElement :: P (Either Literal Identifier)
arrayElement = fmap Left literal P.<|> fmap Right identifier -- lexeme

-- * 3.4.7 Reserved Identifiers

-- | These are the reserved identifiers.
stReservedIdentifiers :: [String]
stReservedIdentifiers = words "nil true false self super"

-- * 3.5.1

-- | character ::= "Any character in the implementation-defined character set"
character :: P Char
character = P.anyChar

-- | whitespace ::= "Any non-printing character interpreted as white space including spaces, tabs, and line breaks"
whitespace :: P Char
whitespace = P.space

-- | digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
digit :: P Char
digit = P.digit

-- | uppercaseAlphabetic ::= 'A' .. 'Z'
uppercaseAlphabetic :: P Char
uppercaseAlphabetic = P.upper

-- | lowercaseAlphabetic ::= 'a' .. 'z'
lowercaseAlphabetic :: P Char
lowercaseAlphabetic = P.lower

-- | nonCaseLetter ::= '_'
nonCaseLetter :: P Char
nonCaseLetter = P.char '_'

-- | letter ::= uppercaseAlphabetic | lowercaseAlphabetic | nonCaseLetter | "implementation defined letters"
letter :: P Char
letter = P.choice [uppercaseAlphabetic, lowercaseAlphabetic, nonCaseLetter]

-- * 3.5.2

-- | commentDelimiter ::= '"'
commentDelimiter :: P Char
commentDelimiter = P.char '"' -- non-lexeme

{- | nonCommentDelimiter::= "any character that is not a commentDelimiter "

> stParse nonCommentDelimiter "x" == 'x'
> stParse nonCommentDelimiter "\"" -- FAIL
-}
nonCommentDelimiter :: P Char
nonCommentDelimiter = P.noneOf ['"']

{- | comment := commentDelimiter nonCommentDelimiter * commentDelimiter

> stParse comment "\"\"" == ""
> stParse comment "\" x\"" == " x"
> stParse comment "\"x \"" == "x "
> stParse comment "\"analog bubbles (jmcc) #1\"" == "analog bubbles (jmcc) #1"
-}
comment :: P String
comment = P.between commentDelimiter (lexeme commentDelimiter) (P.many nonCommentDelimiter)

-- * 3.5.3

type ReservedIdentifier = String

-- | One of stReservedIdentifiers
reservedIdentifier :: P ReservedIdentifier
reservedIdentifier = P.choice (map P.string stReservedIdentifiers)

type OrdinaryIdentifier = String

ordinaryIdentifier :: P OrdinaryIdentifier
ordinaryIdentifier = P.identifier stLexer

type Identifier = String

{- | identifier ::= letter (letter | digit)*

> stParse identifier "x1" == "x1"
> stParse identifier "1x" -- FAIL
> stParse identifier "" -- FAIL
> stParse identifier "true" == "true"
-}
identifier :: P Identifier
identifier = ordinaryIdentifier P.<|> reservedIdentifier

-- * 3.5.4

type Keyword = String

{- | keyword ::= identifier ':'

> stParse keyword "kw:" == "kw:"
> stParse keyword "kw" -- FAIL
-}
keyword :: P Keyword
keyword = do
  cs <- P.label identifier "keyword"
  c <- P.label (P.char ':') "keyword"
  P.optional separator -- lexeme
  return (cs ++ [c])

-- * 3.5.5

-- | binaryCharacter ::= '!' | '%' | '&'' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'
binaryCharacter :: P Char
binaryCharacter = P.oneOf "!%&*+,/<=>?@\\~|-"

type BinarySelector = String

-- | binarySelector ::= binaryCharacter+
--
-- > stParse binarySelector "+" == "+"
-- > stParse binarySelector "+p" == "+" -- +1 must parse as BinarySelector=+ BinaryArgument=1
binarySelector :: P BinarySelector
binarySelector = P.operator stLexer

-- | returnOperator ::= '^'
returnOperator :: P Char
returnOperator = lexeme (P.char '^')

-- | assignmentOperator ::= ':='
assignmentOperator :: P String
assignmentOperator = lexeme (P.string ":=")

-- * 3.5.6

-- | integer ::= decimalInteger | radixInteger
integer :: P Integer
integer = decimalInteger

-- | decimalInteger ::= digits
decimalInteger :: P Integer
decimalInteger = fmap read digits

-- | digits ::= digit+
digits :: P String
digits = P.many1 P.digit

-- | float ::= mantissa [exponentLetter exponent]
float :: P Double
float = P.float stLexer

-- * 3.5.7

type QuotedCharacter = Char

quotedCharacter_pp :: QuotedCharacter -> String
quotedCharacter_pp = printf "$%c"

{- | quotedCharacter ::= '$' character

> stParse quotedCharacter "$x" == 'x'
-}
quotedCharacter :: P Char
quotedCharacter = P.label (P.char '$' >> lexeme P.anyChar) "quotedCharacter"

-- * 3.5.8

type QuotedString = String

quotedString_pp :: QuotedString -> String
quotedString_pp = printf "'%s'"

{- | quotedString ::= stringDelimiter stringBody stringDelimiter

> stParse quotedString "''" == ""
> stParse quotedString "' xy'" == " xy"
> stParse quotedString "'x''y'" == "x'y"
> stParse quotedString "'''x''y'''" == "'x'y'"
-}
quotedString :: P QuotedString
quotedString = P.label (P.between stringDelimiter (lexeme stringDelimiter) stringBody) "quotedString" -- lexeme

{- | stringBody ::= (nonStringDelimiter | (stringDelimiter stringDelimiter)*)

> stParse stringBody "" == ""
> stParse stringBody "x" == "x"
> stParse stringBody "''" == "'"
> stParse stringBody "x''y" = "x'y"
> stParse stringBody "x'" -- ?
-}
stringBody :: P String
stringBody = P.many (nonStringDelimiter P.<|> P.try (stringDelimiter >>~ stringDelimiter))

-- | stringDelimiter ::= ''' "a single quote"
stringDelimiter :: P Char
stringDelimiter = P.char '\'' -- non-lexeme

-- | nonStringDelimiter ::= "any character except stringDelimiter"
nonStringDelimiter :: P Char
nonStringDelimiter = P.noneOf "'"

-- * 3.5.9

type HashedString = String

hashedString_pp :: HashedString -> String
hashedString_pp = printf "#'%s'"

{- | hashedString ::= '#' quotedString

> stParse hashedString "#'x'" == "x"
-}
hashedString :: P HashedString
hashedString = (P.char '#' >> quotedString) P.<?> "hashedString"

-- * 3.5.10

{- | Identifier (lexeme), not ending with ':'

> stParse unarySelector "p" == "p"
> stParse unarySelector "p:" -- FAIL
-}
unarySelector :: P Identifier
unarySelector = (identifier >>= \u -> P.notFollowedBy (P.char ':') >> return u) P.<?> "unarySelector"

data Selector
  = UnarySelector Identifier
  | BinarySelector Identifier
  | KeywordSelector Identifier
  deriving (Eq, Show)

selector_pp :: Selector -> String
selector_pp sel =
  case sel of
    UnarySelector u -> u
    BinarySelector b -> b
    KeywordSelector k -> k

{- | quotedSelector ::= '#' (unarySelector | binarySelector | keywordSelector)

> stParse quotedSelector "#abs" == UnarySelector "abs"
> stParse quotedSelector "#freq:" == KeywordSelector "freq:"
> stParse quotedSelector "#freq:iphase:" == KeywordSelector "freq:iphase:"
> stParse quotedSelector "#+" == BinarySelector "+"
> stParse quotedSelector "#+:" -- FAIL
> stParse quotedSelector "#+x" -- FAIL
-}
quotedSelector :: P Selector
quotedSelector =
  let sel = P.choice [fmap BinarySelector binarySelector, fmap KeywordSelector (P.try keywordSelector), fmap UnarySelector unarySelector]
  in P.label (P.char '#' >> sel) "quotedSelector" -- lexeme

{- | keywordSelector ::= keyword+

> stParse keywordSelector "freq:" == "freq:"
> stParse keywordSelector "freq:iphase:" == "freq:iphase:"
> stParse keywordSelector "freq:iphase:x" -- FAIL
-}
keywordSelector :: P Identifier
keywordSelector = fmap concat (P.many1 keyword) P.<?> "keywordSelector"

-- * 3.5.11

{- | separator ::= (whitespace | comment)*

> stParse separator " " == " "
> stParse separator "\t" == "\t"
> stParse separator "\n" == "\n"
> stParse separator "  " == "  "
> stParse separator " \t" == " \t"
> stParse separator "\"commentary\"" == "commentary"
> stParse separator "\t\"commentary\"" == "\tcommentary"
> stParse separator "" -- FAIL
-}
separator :: P String
separator = fmap concat (P.many1 (fmap return P.space  P.<|> comment) P.<?> "separator")
