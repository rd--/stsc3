-- | Parser and pretty printer for a subset of ANSI Smalltalk.
module Language.Smalltalk.Ansi where

import Data.Functor.Identity {- base -}

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

{-
import Data.Char {- base -}
deleteLeadingSpaces :: String -> String
deleteLeadingSpaces = dropWhile isSpace
-}

-- | Run parser and report any error.  Does not delete leading spaces.
stParse :: P t -> String -> t
stParse p = either (\m -> error ("stParse: " ++ show m)) id . P.parse p ""

-- * 3.3 Smalltalk Abstract Program Grammar

-- * 3.3.1 Program Definition

data SmalltalkProgram =
  SmalltalkProgram {programElements :: [ProgramElement]}
  deriving (Eq, Show)

-- | <<Smalltalk program>> ::= <<program element>>+ <<initialization ordering>>
smalltalkProgram :: P SmalltalkProgram
smalltalkProgram = P.optional separator >> fmap SmalltalkProgram (P.many1 programElement)

data ProgramElement
  = ProgramGlobal GlobalDefinition
  | ProgramInitializer ProgramInitializerDefinition
  deriving (Eq,Show)

-- | <<program element>> ::= <<class definition>> | <<global definition>> | <<pool definition>> | <<program initializer definition>>
programElement :: P ProgramElement
programElement = P.choice [fmap ProgramInitializer programInitializerDefinition
                          ,fmap ProgramGlobal globalDefinition]

-- * 3.3.2 Class Definition

data Indexable = ByteIndexable | ObjectIndexable | NonIndexable deriving (Eq,Show)

data InstanceState = InstanceState Indexable [Identifier] deriving (Eq,Show)

noInstanceState :: InstanceState
noInstanceState = InstanceState NonIndexable []

-- | A class definition defines the behavior and encapsulated state of objects.
data ClassDefinition =
  ClassDefinition {className :: Identifier
                  ,superclassName :: Maybe Identifier
                  ,instanceState :: InstanceState
                  ,classInstanceVariableNames :: [Identifier]
                  ,classVariableNames :: [Identifier]
                  ,importedPoolNames :: [Identifier]
                  ,instanceMethods :: [MethodDefinition]
                  ,classMethods :: [MethodDefinition]
                  ,classInitializer :: Maybe InitializerDefinition}
  deriving (Eq,Show)

-- * 3.3.3 Global Variable Definition

data GlobalDefinition =
  GlobalDefinition GlobalName (Maybe VariableInitializer)
  deriving (Eq, Show)

-- | <<global definition>> ::= [<<constant designator>>] <<global name>> [<<variable initializer>>]
globalDefinition :: P GlobalDefinition
globalDefinition = do
  -- constant designator
  n <- identifier
  v <- P.optionMaybe initializerDefinition
  return (GlobalDefinition n v)

-- | <<global name>> ::= identifier
type GlobalName = Identifier

-- | <<variable initializer>> ::= <initializer definition>
type VariableInitializer = InitializerDefinition

-- * 3.3.5 Program Initializer Definition

type ProgramInitializerDefinition = InitializerDefinition

{- | <<program initializer definition >> ::= <initializer definition>

> stParse programInitializerDefinition "\"x\" |t| t + 1"
> stParse programInitializerDefinition "" -- FAIL
-}
programInitializerDefinition :: P ProgramInitializerDefinition
programInitializerDefinition = initializerDefinition

-- * 3.4.2

data MethodDefinition =
  MethodDefinition Pattern (Maybe Temporaries) (Maybe Statements)
  deriving (Eq,Show)

{- | <method definition> ::= <message pattern> [<temporaries>] [<statements>]

> stParse methodDefinition "p"
> stParse methodDefinition "p q"
> stParse methodDefinition "p ^q"
> stParse methodDefinition "p q. ^r"
> stParse methodDefinition "p |t|"
> stParse methodDefinition "p |t| r"
> stParse methodDefinition "p |t| r. ^s"
> stParse methodDefinition "p: q"
> stParse methodDefinition "p: q r"
> stParse methodDefinition "p: q ^r"
> stParse methodDefinition "p: q r. ^s"
> stParse methodDefinition "printElementsOn: aStream aStream nextPut: $(."
> stParse methodDefinition "* anObject ^self shallowCopy *= anObject"

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
  | BinaryPattern BinaryIdentifier Identifier
  | KeywordPattern [(Keyword,Identifier)]
  deriving (Eq, Show)

{- | Derive method selector from Pattern.
     Return is either Identifier or BinarySelector (both Strings).

> patternSelector (stParse messagePattern "midicps") == "midicps"
> patternSelector (stParse messagePattern "+ aNumber") == "+"
> patternSelector (stParse messagePattern "new: x") == "new:"
> patternSelector (stParse messagePattern "freq: f phase: p") == "freq:phase:"
-}
patternSelector :: Pattern -> Identifier
patternSelector pat =
  case pat of
    UnaryPattern u -> u
    BinaryPattern b _ -> b
    KeywordPattern kp -> concatMap fst kp

methodSelector :: MethodDefinition -> Identifier
methodSelector (MethodDefinition p _ _) = patternSelector p

{- | <message pattern> ::= <unary pattern> | <binary pattern> | <keyword pattern>

> stParse messagePattern "p" == UnaryPattern "p"
> stParse messagePattern "+p" == stParse messagePattern "+ p"
> stParse messagePattern "k1:p1" == stParse messagePattern "k1: p1"
> stParse messagePattern "k1:p1 k2:p2" == stParse messagePattern "k1: p1 k2: p2"
> stParse messagePattern "k: v x"
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
  return (BinaryPattern sel arg)

{- | <keyword pattern> ::= (keyword <method argument>)+

> stParse keywordPattern "k: p" == KeywordPattern [("k:","p")]
> stParse keywordPattern "k1: p1 k2: p2" == KeywordPattern [("k1:","p1"),("k2:","p2")]
> stParse keywordPattern "k1: p1 ..." == KeywordPattern [("k1:","p1")]
-}
keywordPattern :: P Pattern
keywordPattern = do
  let f = do kw <- keyword
             arg <- identifier
             return (kw,arg)
  fmap KeywordPattern (P.many1 (P.try f))

data Temporaries = Temporaries [Identifier] deriving (Eq, Show)

verticalBar :: P Char
verticalBar = lexeme (P.char '|')

temporariesIdentifierSequence :: P [Identifier]
temporariesIdentifierSequence = P.between verticalBar verticalBar temporary_variable_list

{- | <temporaries> ::= '|' <temporary variable list> '|'

> stParse temporaries "||"
> stParse temporaries "|p|"
> stParse temporaries "| p|"
> stParse temporaries "|p |"
> stParse temporaries "|p q r|"
-}
temporaries :: P Temporaries
temporaries = P.label (fmap Temporaries temporariesIdentifierSequence) "temporaries"

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

data InitializerDefinition =
  InitializerDefinition (Maybe Temporaries) (Maybe Statements)
  deriving (Eq,Show)

{- | <initializer definition> ::= [<temporaries>] [<statements>]

> stParse initializerDefinition "|a b| a := 1 . ^ a + b ."
> stParse initializerDefinition "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> stParse initializerDefinition "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> stParse initializerDefinition "|a b c| a := 1 . b := 2 . c := 3 . ^ a + b + c ."
> stParse initializerDefinition "|a b c| a := [1] . b := [2] . c := [3] . ^ a value + b value + c value ."
> stParse initializerDefinition "[:x | x * x] value: Float pi * 2"
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

data BlockBody =
  BlockBody (Maybe [BlockArgument]) (Maybe Temporaries) (Maybe Statements)
  deriving (Eq,Show)

{- | <block body> ::= [<block argument>* '|'] [<temporaries>] [<statements>]

> stParse blockBody ""
> stParse blockBody ":a|" == stParse blockBody ": a|"
> stParse blockBody ":a :b|" == stParse blockBody ": a : b|"
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
  P.optional separator -- ?
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  return (BlockBody a t s)

-- | An identifier for a block argument.  Written with a ':' prefix.
type BlockArgument = Identifier

{- | <block argument> ::= ':' identifier

> stParse blockArgument ":a" == stParse blockArgument ": a"
-}
blockArgument :: P BlockArgument
blockArgument = lexeme (P.char ':') >> identifier

-- * 3.4.5

data Statements
  = StatementsReturn ReturnStatement
  | StatementsExpression Expression (Maybe Statements)
  deriving (Eq,Show)

-- | Prepend a list of expressions, as statements, to an existing statement.
expressionSequenceToStatements :: Maybe Statements -> [Expression] -> Statements
expressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "expressionSequenceToStatements"
          [e0] -> StatementsExpression e0 stm
          e0:eN -> StatementsExpression e0 (Just (f eN))
  in f

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
> stParse statements "p . ^q" == stParse statements "p . ^ q"
> stParse statements "^ 440 * (2 ** ((self - 69) * (1 / 12)))"
> stParse statements "p. q." == stParse statements "p. q"
-}
statements :: P Statements
statements = do
  let rhs = do
        e <- expression
        p <- P.optionMaybe period
        s <- case p of
               Nothing -> return Nothing
               Just _ -> P.optionMaybe statements
        return (StatementsExpression e s)
  P.choice [fmap StatementsReturn (returnStatement >>~ P.optional period),rhs]

data ReturnStatement = ReturnStatement Expression deriving (Eq,Show)

{- | <return statement> ::= returnOperator <expression>

> stParse returnStatement "^1"
> stParse returnStatement "^p"
> stParse returnStatement "^ 1"
> stParse returnStatement "^ p"
> stParse returnStatement "^ a value + b value + c value"
> stParse returnStatement "^ self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
-}
returnStatement :: P ReturnStatement
returnStatement = fmap ReturnStatement (returnOperator >> expression)

data Expression = ExprAssignment Assignment | ExprBasic BasicExpression deriving (Eq, Show)

expressionEither :: (Assignment -> t) -> (BasicExpression -> t) -> Expression -> t
expressionEither f g e =
  case e of
    ExprAssignment x -> f x
    ExprBasic x -> g x

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
> stParse expression "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
-}
expression :: P Expression
expression = fmap ExprAssignment (P.try assignment) P.<|> fmap ExprBasic basicExpression

data Assignment = Assignment Identifier Expression deriving (Eq,Show)

{- | <assignment> ::= <assignment target> assignmentOperator <expression>

> stParse assignment "p:=1"
> stParse assignment "p :=1"
> stParse assignment "p:= 1"
> stParse assignment "p := 1"
> stParse assignment "p := q"
> stParse assignment "p := 2.0"
> stParse assignment "p := 'x'"
> stParse assignment "p := 8 mixFill: [:i| |n trigger pluck freq z metal| n := 15 ]"
> stParse assignment "p := q := r := nil"
-}
assignment :: P Assignment
assignment = do
  a <- assignmentTarget
  _ <- assignmentOperator
  e <- expression
  return (Assignment a e)

data BasicExpression =
  BasicExpression Primary (Maybe Messages) (Maybe CascadedMessages)
  deriving (Eq, Show)

{- | <basic expression> ::= <primary> [<messages> <cascaded messages>]

> stParse basicExpression "1"
> stParse basicExpression "p"
> stParse basicExpression "1 negate"
> stParse basicExpression "p negate"
> stParse basicExpression "1 + 2"
> stParse basicExpression "(p)"
> stParse basicExpression "(p q)"
> stParse basicExpression "a value + b value + c value"
> stParse basicExpression "p q r: x" == stParse basicExpression "p q r:x"
> stParse basicExpression "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
> stParse basicExpression "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
> stParse basicExpression "w * ((x + y) z)"
> stParse basicExpression "w * (x + y) z"
-}
basicExpression :: P BasicExpression
basicExpression = do
  p <- primary
  m <- P.optionMaybe messages
  c <- cascadedMessages
  return (BasicExpression p m (if null c then Nothing else Just c))

-- | <assignment target> := identifier
assignmentTarget :: P Identifier
assignmentTarget = P.label identifier "assignmentTarget"

data Messages
  = MessagesUnary [UnaryMessage] (Maybe [BinaryMessage]) (Maybe KeywordMessage)
  | MessagesBinary [BinaryMessage] (Maybe KeywordMessage)
  | MessagesKeyword KeywordMessage
  deriving (Eq,Show)

data Primary
  = PrimaryIdentifier Identifier
  | PrimaryLiteral Literal
  | PrimaryBlock BlockBody
  | PrimaryExpression Expression
  | PrimaryArrayExpression [BasicExpression] -- NON-ANSI
  deriving (Eq, Show)

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
primary =
  lexeme (P.choice [fmap PrimaryBlock blockConstructor
                   ,fmap PrimaryExpression (inParentheses expression)
                   ,fmap PrimaryArrayExpression (inBraces (P.sepBy basicExpression period))
                   ,fmap PrimaryIdentifier identifier
                   ,fmap PrimaryLiteral literal] P.<?> "primary")

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
  b <- P.many binaryMessage
  P.optional separator -- ?
  k <- P.optionMaybe keywordMessage
  return (MessagesUnary u (if null b then Nothing else Just b) k)

-- | <binary message>+ [<keyword message>]
binaryMessages :: P Messages
binaryMessages = do
  b <- P.many1 binaryMessage
  k <- P.optionMaybe keywordMessage
  return (MessagesBinary b k)

{- | <messages> ::= (<unary message>+ <binary message>* [<keyword message>] ) | (<binary message>+ [<keyword message>] ) | <keyword message>

> rw = messages_pp . stParse messages
> (rw "k1:p1",rw "k1:p1 k2:p2")
> (rw "+p",rw "+p +q")
> (rw "+p k:r",rw "+p +q k:r")

> stParse messages "k1:p1" == stParse messages "k1: p1"
> stParse messages "k1: p1 k2: p2" == stParse messages "k1:p1 k2:p2"
> stParse messages "+1"== stParse messages "+ 1"
> stParse messages "+p" == stParse messages "+ p"
> stParse messages "+p +q" == stParse messages "+ p + q"
> stParse messages "+p +q k:r" == stParse messages "+ p + q k: r"
> stParse messages "q r: x" == stParse messages "q r:x"
> stParse messages "< 0 ifTrue: [0 - self] ifFalse: [self]" == stParse messages "<0ifTrue:[0-self]ifFalse:[self]"
-}
messages :: P Messages
messages = P.choice [P.try binaryMessages,P.try (fmap MessagesKeyword keywordMessage),unaryMessages]

data UnaryMessage = UnaryMessage Identifier deriving (Eq,Show)

unaryMessageSelector :: UnaryMessage -> Identifier
unaryMessageSelector (UnaryMessage u) = u

data BinaryMessage =
  BinaryMessage Identifier BinaryArgument
  deriving (Eq,Show)

binaryMessageSelector :: BinaryMessage -> Identifier
binaryMessageSelector (BinaryMessage b _) = b

data KeywordMessage =
  KeywordMessage [(Keyword,KeywordArgument)]
  deriving (Eq,Show)

keywordMessageSelector :: KeywordMessage -> Identifier
keywordMessageSelector (KeywordMessage l) = concatMap fst l

{- | <unary message> ::= unarySelector

> stParse unaryMessage "p" == UnaryMessage "p"
> stParse unaryMessage "" -- FAIL
-}
unaryMessage :: P UnaryMessage
unaryMessage = fmap UnaryMessage unarySelector -- lexeme

{- | <binary message> ::= binarySelector <binary argument>

> stParse binaryMessage "+p" == stParse binaryMessage "+ p"
> stParse binaryMessage "+1" == stParse binaryMessage "+ 1"
> stParse binaryMessage "+1.0" == stParse binaryMessage "+ 1.0"
> stParse binaryMessage "+ 1 k:" -- + 1
-}
binaryMessage :: P BinaryMessage
binaryMessage = do
  sel <- binarySelector
  arg <- binaryArgument
  return (BinaryMessage sel arg)

data BinaryArgument =
  BinaryArgument Primary (Maybe [UnaryMessage])
  deriving (Eq, Show)

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
  return (BinaryArgument p u)

{- | <keyword message> ::= (keyword <keyword argument>)+

> stParse keywordMessage "k:p" == stParse keywordMessage "k: p"
> stParse keywordMessage "k1:p1 k2:p2" == stParse keywordMessage "k1: p1 k2: p2"
-}
keywordMessage :: P KeywordMessage
keywordMessage = do
  let f = do kw <- keyword
             arg <- keywordArgument
             return (kw,arg)
  fmap KeywordMessage (P.many1 f)

data KeywordArgument =
  KeywordArgument Primary (Maybe [UnaryMessage]) (Maybe [BinaryMessage])
  deriving (Eq, Show)

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
  return (KeywordArgument p u b)

cascadeSeparator :: P String
cascadeSeparator = P.semi stLexer

type CascadedMessages = [Messages]

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
> stParse literal "#(x y z)"
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
  fmap (NumberLiteral . numberEither (Int . rw) (Float . rw)) number -- lexeme

data Number = Int Integer | Float Double deriving (Eq, Show)

numberEither :: (Integer -> t) -> (Double -> t) -> Number -> t
numberEither f1 f2 n =
  case n of
    Int x -> f1 x
    Float x -> f2 x

{- | <number> ::= integer | float | scaledDecimal

> map (stParse number) (words "1 1.2")
> map (stParse number) (words "-1 -1.2") -- FAIL
-}
number :: P Number
number = fmap Float (P.try float) P.<|> fmap Int integer -- lexeme

-- | <character literal> ::= quotedCharacter
characterLiteral :: P Literal
characterLiteral = fmap CharacterLiteral quotedCharacter

{- | <string literal> ::= quotedString

> stParse stringLiteral "'x'" == StringLiteral "x"
-}
stringLiteral :: P Literal
stringLiteral = fmap StringLiteral quotedString

-- | <symbol literal> ::= hashedString
symbolLiteral :: P Literal
symbolLiteral = fmap SymbolLiteral hashedString

-- | <selector literal> ::= quotedSelector
selectorLiteral :: P Literal
selectorLiteral = fmap SelectorLiteral quotedSelector

hashOpenParen :: P String
hashOpenParen = lexeme (P.string "#(")

closeParen :: P Char
closeParen = lexeme (P.char ')')

{- | <array literal> ::= '#(' <array element>* ')'

> stParse arrayLiteral "#()" == stParse arrayLiteral "#( )"
> stParse arrayLiteral "#(1)" == stParse arrayLiteral "#( 1 )"
> stParse arrayLiteral "#(1 2.0)"
> stParse arrayLiteral "#(1 2.0 3)"
> stParse arrayLiteral "#(1 2.0 p)"
> stParse arrayLiteral "#(1 #(2 3) 4)"
-}
arrayLiteral :: P Literal
arrayLiteral = fmap ArrayLiteral (P.between hashOpenParen closeParen (P.many arrayElement))

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
commentDelimiter = P.char '"' -- non-lexeme (the lhs comment marker is not a lexeme, the rhs is)

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

{- | One of stReservedIdentifiers

> map (stParse reservedIdentifier) (words "self super nil")
-}
reservedIdentifier :: P ReservedIdentifier
reservedIdentifier = lexeme (P.choice (map (P.try . P.string) stReservedIdentifiers))

type OrdinaryIdentifier = String

ordinaryIdentifier :: P OrdinaryIdentifier
ordinaryIdentifier = P.identifier stLexer

type Identifier = String

{- | identifier ::= letter (letter | digit)*

> stParse identifier "x1" == "x1"
> stParse identifier "X1" == "X1"
> stParse identifier "1x" -- FAIL
> stParse identifier "" -- FAIL
> stParse identifier "true" == "true"
> stParse identifier "nil" == "nil"
-}
identifier :: P Identifier
identifier = ordinaryIdentifier P.<|> reservedIdentifier

-- * 3.5.4

-- | An identifier ending with a colon.
type Keyword = Identifier

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

-- | Identifier for binary operations.
type BinaryIdentifier = Identifier

-- | binarySelector ::= binaryCharacter+
--
-- > stParse binarySelector "+" == "+"
-- > stParse binarySelector "+p" == "+" -- +1 must parse as BinarySelector=+ BinaryArgument=1
binarySelector :: P BinaryIdentifier
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
digits = lexeme (P.many1 P.digit)

-- | float ::= mantissa [exponentLetter exponent]
float :: P Double
float = P.float stLexer

-- * 3.5.7

type QuotedCharacter = Char

{- | quotedCharacter ::= '$' character

> stParse quotedCharacter "$x" == 'x'
-}
quotedCharacter :: P Char
quotedCharacter = P.label (P.char '$' >> lexeme P.anyChar) "quotedCharacter"

-- * 3.5.8

type QuotedString = String

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
stringDelimiter = P.char '\'' -- non-lexeme (the lhs is not a lexeme, the rhs is)

-- | nonStringDelimiter ::= "any character except stringDelimiter"
nonStringDelimiter :: P Char
nonStringDelimiter = P.noneOf "'"

-- * 3.5.9

type HashedString = String

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
  | BinarySelector BinaryIdentifier
  | KeywordSelector Identifier
  deriving (Eq, Show)

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
keywordSelector :: P Keyword
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
