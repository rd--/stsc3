{- | Ast and parser for a subset of ANSI Smalltalk.
     The file is organised according to the report.
     Numbers refer to sections of the report.
     <https://wiki.squeak.org/squeak/uploads/172/standard_v1_9-indexed.pdf>

SmalltalkProgram ProgramElement (3.3.1)
ClassDefinition (3.3.2)
GlobalDefinition (3.3.3)
ProgramInitializerDefinition (3.3.5)
MethodDefinition, Temporaries (3.4.2)
InitializerDefinition (3.4.3)
BlockBody (3.4.4)
Statements ReturnStatement Expression Assignment BasicExpression Primary Messages (3.4.5)
Literal (3.4.6)

-}
module Language.Smalltalk.Ansi where

import Data.Functor.Identity {- base -}

import Data.List.Split {- split -}

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

-- | Sequence of program elements.
data SmalltalkProgram =
  SmalltalkProgram {programElements :: [ProgramElement]}
  deriving (Eq, Show)

{- | <<Smalltalk program>> ::= <<program element>>+ <<initialization ordering>>

> p = stParse smalltalkProgram
> p "" == SmalltalkProgram {programElements = []}
> p "self ifNil:" -- ? error
-}
smalltalkProgram :: P SmalltalkProgram
smalltalkProgram = do
  P.optional separator
  fmap SmalltalkProgram (P.many1 nonEmptyProgramElement P.<|> (P.eof >> return []))

{-
import Control.Monad.Loops {- monad-loops -}

{- | P.many1 cannot accept a parser that can match an empty string, which programElement can.
     This attempts parse ProgramElements until eof.
     However in the case of a syntax error this will not halt.
-}
programElementSequence :: P [ProgramElement]
programElementSequence = Control.Monad.Loops.unfoldM maybeProgramElement

maybeProgramElement :: P (Maybe ProgramElement)
maybeProgramElement = (P.eof >> return Nothing) P.<|> fmap Just programElement
-}

-- | 3.3.1
data ProgramElement
  = ProgramGlobal GlobalDefinition
  | ProgramInitializer ProgramInitializerDefinition
  deriving (Eq,Show)

{- | <<program element>> ::= <<class definition>>
                           | <<global definition>>
                           | <<pool definition>>
                           | <<program initializer definition>>
-}
programElement :: P ProgramElement
programElement =
  P.choice [fmap ProgramInitializer programInitializerDefinition
           ,fmap ProgramGlobal globalDefinition]

nonEmptyProgramElement :: P ProgramElement
nonEmptyProgramElement =
  P.choice [fmap ProgramInitializer nonEmptyProgramInitializerDefinition
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

metaclassName :: Identifier -> Identifier
metaclassName x = x ++ " class"

{- | The name of the meta class, ie. the class name with a class suffix.
     Metaclasses do not have separate ClassDefinitions.
     In Smalltalk the rule is that the class of a metaclass is "Metaclass".
     This includes the class of "Metaclass class", forming a cycle.
     This does not arise here since "Metaclass class" does not have a ClassDefinition.
-}
classMetaclassName :: ClassDefinition -> Identifier
classMetaclassName = metaclassName . className

-- | "Smalltalk implementations have traditionally open-coded certain
-- messages including those with the following selectors."
restrictedSelectors :: [Identifier]
restrictedSelectors =
  ["ifTrue:","ifTrue:ifFalse:","ifFalse:","ifFalse:ifTrue:"
  ,"to:do:","to:by:do:"
  ,"and:","or:","=="
  ,"timesRepeat:"
  ,"basicAt:","basicAt:put:","basicSize","basicNew:"]

-- * 3.3.3 Global Variable Definition

-- | 3.3.3
data GlobalDefinition =
  GlobalDefinition GlobalName (Maybe VariableInitializer)
  deriving (Eq, Show)

-- | <<global name>> ::= identifier
type GlobalName = Identifier

-- | <<variable initializer>> ::= <initializer definition>
type VariableInitializer = InitializerDefinition

{- | <<global definition>> ::= [<<constant designator>>] <<global name>> [<<variable initializer>>]

> p = stParse globalDefinition
> p "g"
> p "g g := 0"
-}
globalDefinition :: P GlobalDefinition
globalDefinition = do
  -- constant designator
  n <- identifier
  v <- P.optionMaybe initializerDefinition
  return (GlobalDefinition n v)

-- * 3.3.5 Program Initializer Definition

-- | 3.3.5
type ProgramInitializerDefinition = InitializerDefinition

{- | <<program initializer definition >> ::= <initializer definition>

> p = stParse programInitializerDefinition
> p "|t| t + 1"
> p "\"x\" |t| t + 1"
> p "" == InitializerDefinition Nothing Nothing
-}
programInitializerDefinition :: P ProgramInitializerDefinition
programInitializerDefinition = initializerDefinition

-- | A variant that does will not match the empty string.
nonEmptyProgramInitializerDefinition :: P ProgramInitializerDefinition
nonEmptyProgramInitializerDefinition = nonEmptyInitializerDefinition

-- * 3.4.2

-- | 3.4.2
data MethodDefinition =
  MethodDefinition Pattern (Maybe Temporaries) (Maybe Statements)
  deriving (Eq,Show)

-- | Does MethodDefinition end with a Return (local).
methodDefinitionHasReturn :: MethodDefinition -> Bool
methodDefinitionHasReturn x =
  case x of
    MethodDefinition _ _ Nothing -> False
    MethodDefinition _ _ (Just s) -> statementsHasReturn s

{- | <method definition> ::= <message pattern> [<temporaries>] [<statements>]

> p = stParse methodDefinition
> p "p"
> p "p q"
> p "p ^q"
> p "p q. ^r"
> p "p |t|"
> p "p |t| r"
> p "p |t| r. ^s"
> p "p: q"
> p "p: q r"
> p "p: q ^r"
> p "p: q r. ^s"
> p "printElementsOn: aStream aStream nextPut: $(."
> p "* anObject ^self shallowCopy *= anObject"

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

> p = stParse messagePattern
> patternSelector (p "midicps") == UnarySelector "midicps"
> patternSelector (p "+ aNumber") == BinarySelector "+"
> patternSelector (p "new: x") == KeywordSelector "new:"
> patternSelector (p "freq: f phase: p") == KeywordSelector "freq:phase:"
-}
patternSelector :: Pattern -> Selector
patternSelector pat =
  case pat of
    UnaryPattern u -> UnarySelector u
    BinaryPattern b _ -> BinarySelector b
    KeywordPattern kp -> KeywordSelector (concatMap fst kp)

{- | Derive argument list from Pattern.

> map patternArguments [UnaryPattern "x",BinaryPattern "+" "x",KeywordPattern [("x:","p"),("y:","q")]]
-}
patternArguments :: Pattern -> [Identifier]
patternArguments pat =
  case pat of
    UnaryPattern _ -> []
    BinaryPattern _ p -> [p]
    KeywordPattern kp -> map snd kp

-- | Derive method selector from definition.
methodSelector :: MethodDefinition -> Selector
methodSelector (MethodDefinition p _ _) = patternSelector p

-- | Untyped identifier for method selector.
methodSignature :: MethodDefinition -> Identifier
methodSignature = selectorIdentifier . methodSelector

{- | <message pattern> ::= <unary pattern> | <binary pattern> | <keyword pattern>

> p = stParse messagePattern
> p "p" == UnaryPattern "p"
> p "+p" == p "+ p"
> p "k1:p1" == p "k1: p1"
> p "k1:p1 k2:p2" == p "k1: p1 k2: p2"
> p "k: v x" == p "k:v"
-}
messagePattern :: P Pattern
messagePattern =
  P.choice [P.try keywordPattern
           ,P.try binaryPattern
           ,unaryPattern] P.<?> "messagePattern"

{- | <unary pattern> ::= unarySelector

> stParse unaryPattern "p"
> stParse unaryPattern "p:" -- error
-}
unaryPattern :: P Pattern
unaryPattern = fmap (UnaryPattern . selectorIdentifier) unarySelector -- lexeme

{- | <binary pattern> ::= binarySelector <method argument>

> stParse binaryPattern "+p" == stParse binaryPattern "+ p"
-}
binaryPattern :: P Pattern
binaryPattern = do
  BinarySelector sel <- binarySelector
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

-- | 3.4.2
data Temporaries = Temporaries [Identifier] deriving (Eq, Show)

-- | Number of temporaries.
temporariesLength :: Temporaries -> Int
temporariesLength (Temporaries x) = length x

-- | Empty Temporaries list.
emptyTemporaries :: Temporaries
emptyTemporaries = Temporaries []

-- | Vertical bar as lexeme (token).
verticalBar :: P Char
verticalBar = lexeme (P.char '|')

temporariesIdentifierSequence :: P [Identifier]
temporariesIdentifierSequence = P.between verticalBar verticalBar temporary_variable_list

{- | <temporaries> ::= '|' <temporary variable list> '|'

> p = stParse temporaries
> p "||" == Temporaries []
> p "|p|" == p "| p|"
> p "|p |" == p "| p |"
> p "|p q r|" == Temporaries ["p","q","r"]
-}
temporaries :: P Temporaries
temporaries = P.label (fmap Temporaries temporariesIdentifierSequence) "temporaries"

{- | <temporary variable list> ::= identifier*

> p = stParse temporary_variable_list
> p ""
> p "p"
> p "p q"
> p "p q r" == p "p q r +"
-}
temporary_variable_list :: P [Identifier]
temporary_variable_list = P.many identifier P.<?> "temporary_variable_list"

-- * 3.4.3

{- | 3.4.3

The value of an initializer with no <statements> is the binding of the reserved identifier 'nil'.
-}
data InitializerDefinition =
  InitializerDefinition (Maybe Temporaries) (Maybe Statements)
  deriving (Eq,Show)

{- | <initializer definition> ::= [<temporaries>] [<statements>]

> p = stParse initializerDefinition
> p "|a b| a := 1 . ^ a + b ."
> p "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> p "SinOsc freq: (69 midicps) phase: 0 mul: 0.1"
> p "|a b c| a := 1 . b := 2 . c := 3 . ^ a + b + c ."
> p "|a b c| a := [1] . b := [2] . c := [3] . ^ a value + b value + c value ."
> p "[:x | x * x] value: Float pi * 2"
> p ""
-}
initializerDefinition :: P InitializerDefinition
initializerDefinition = do
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  return (InitializerDefinition t s)

nonEmptyInitializerDefinition :: P InitializerDefinition
nonEmptyInitializerDefinition = do
  InitializerDefinition t s <- initializerDefinition
  case (t,s) of
    (Nothing,Nothing) -> P.unexpected "nonEmptyInitializerDefinition: empty"
    _ -> return (InitializerDefinition t s)


-- * 3.4.4

{- | <block constructor> ::= '[' <block body> ']'

> p = stParse blockConstructor
> p "[1]" == p "[ 1 ]"
> p "[^1]" ==  p "[ ^ 1 .]"
> p "[:a| |t|]" == p "[ :a | | t | ]"
> p "[:a| |t u| x . y]"
> p "[:a| |t u| t := [:b| |v w| z] . x . y]"
-}
blockConstructor :: P BlockBody
blockConstructor = inBrackets blockBody

data BlockBody =
  BlockBody (Maybe [BlockArgument]) (Maybe Temporaries) (Maybe Statements)
  deriving (Eq,Show)

-- | Does BlockBody end with a Return (non-local).
blockBodyHasReturn :: BlockBody -> Bool
blockBodyHasReturn x =
  case x of
    BlockBody _ _ Nothing -> False
    BlockBody _ _ (Just s) -> statementsHasReturn s

{- | <block body> ::= [<block argument>* '|'] [<temporaries>] [<statements>]

> p = stParse blockBody
> p "" == BlockBody Nothing Nothing Nothing
> p ":a|" == p ": a|"
> p ":a :b|" == p ": a : b|"
> p ":a| |p q r|"
> p ":a :b| |p q r|"
> p "|p q r|"
> p "1"
> p ":a| a + 1"
> p ":a| |b| b := a + 1. ^b"
-}
blockBody :: P BlockBody
blockBody = do
  a <- P.optionMaybe (P.try (P.many1 blockArgument >>~ verticalBar))
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  return (BlockBody a t s)

-- | An identifier for a block argument.  Written with a ':' prefix.
type BlockArgument = Identifier

{- | <block argument> ::= ':' identifier

> p = stParse blockArgument
> p ":a" == p ": a"
-}
blockArgument :: P BlockArgument
blockArgument = lexeme (P.char ':') >> identifier

-- * 3.4.5

-- | 3.4.5
data Statements
  = StatementsReturn ReturnStatement
  | StatementsExpression Expression (Maybe Statements)
  deriving (Eq,Show)

-- | Does Statements end with a Return?
statementsHasReturn :: Statements -> Bool
statementsHasReturn x =
  case x of
    StatementsReturn _ -> True
    StatementsExpression _ Nothing -> False
    StatementsExpression _ (Just x') -> statementsHasReturn x'

-- | Prepend a list of expressions, as statements, to an existing statement.
expressionSequenceToStatements :: Maybe Statements -> [Expression] -> Statements
expressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "expressionSequenceToStatements"
          [e0] -> StatementsExpression e0 stm
          e0:eN -> StatementsExpression e0 (Just (f eN))
  in f

{- | '.' as lexeme (token)

> stParse period "." == "."
-}
period :: P String
period = P.dot stLexer

{- | <statements> ::= (<return statement> ['.']) | (<expression> ['.' [<statements>]])

> p = stParse statements
> p "^p" == p "^ p"
> p "^p." == p "^p ."
> p "^ p." == p "^ p ."
> p "p"
> p "p q"
> p "(p q)"
> p "p.^q" == p "p .^q"
> p "p . ^q" == p "p . ^ q"
> p "^ 440 * (2 ** ((self - 69) * (1 / 12)))"
> p "p. q." == p "p. q"
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

-- | 3.4.5.1 Return statement
data ReturnStatement = ReturnStatement Expression deriving (Eq,Show)

{- | <return statement> ::= returnOperator <expression>

> p = stParse returnStatement
> p "^1"
> p "^p"
> p "^ 1"
> p "^ p"
> p "^ a value + b value + c value"
> p "^ self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
-}
returnStatement :: P ReturnStatement
returnStatement = fmap ReturnStatement (returnOperator >> expression)

-- | 3.4.5.2 Expressions
data Expression = ExprAssignment Assignment | ExprBasic BasicExpression deriving (Eq, Show)

expressionEither :: (Assignment -> t) -> (BasicExpression -> t) -> Expression -> t
expressionEither f g e =
  case e of
    ExprAssignment x -> f x
    ExprBasic x -> g x

{- | <expression> ::= <assignment> | <basic expression>

> p = stParse expression
> p "p := 1"
> p "p := q"
> p "p"
> p "1"
> p "1 + 2"
> p "p := (1 + 2)"
> p "p := (1 + 2) negate"
> p "a value + b value + c value"
> p "note2 := (sequR value: #(-12 -7 -5 0 2 5) value: clock_16) + note1"
> p "freq := ((#(-2 0 3 5 7 10 12 15) at: i) + 60) midicps ."
> p "(1 to: 6) do: [:i| s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand . 0.05 rand} decaytime: 4] . \"rvb\""
> p "out := pitch size mixFill: [:i| |trigger pluck period string| trigger := HPZ1 in: (mousex > (0.25 + ((i - 1) * triggerSpacing))) abs . x]"
> p "out := pitch size mixFill: [:i| x]"
> p "pitch size mixFill: x"
> p "[1] value"
> p "o m: true"
> p "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
-}
expression :: P Expression
expression = fmap ExprAssignment (P.try assignment) P.<|> fmap ExprBasic basicExpression

-- | 3.4.5.2 (Expressions)
data Assignment = Assignment Identifier Expression deriving (Eq,Show)

{- | <assignment> ::= <assignment target> assignmentOperator <expression>

> p = stParse assignment
> p "p:=1" == p "p :=1"
> p "p:= 1" == p "p := 1"
> p "p := q"
> p "p := 2.0"
> p "p := 'x'"
> p "p := 8 mixFill: [:i| |n trigger pluck freq z metal| n := 15 ]"
> p "p := q := r := nil"
-}
assignment :: P Assignment
assignment = do
  a <- assignmentTarget
  _ <- assignmentOperator
  e <- expression
  return (Assignment a e)

-- | 3.4.5.2
data BasicExpression =
  BasicExpression Primary (Maybe Messages) (Maybe CascadedMessages)
  deriving (Eq, Show)

{- | If the expression consists only of a primary, return that.
     If the expression has messages make a PrimaryExpression node.
-}
basicExpressionToPrimary :: BasicExpression -> Primary
basicExpressionToPrimary e =
  case e of
    BasicExpression p Nothing Nothing -> p
    _ -> PrimaryExpression (ExprBasic e)

{- | <basic expression> ::= <primary> [<messages> <cascaded messages>]

> let p = stParse basicExpression
> p "1"
> p "p"
> p "1 negate"
> p "p negate"
> p "1 + 2"
> p "(p)"
> p "(p q)"
> p "a value + b value + c value"
> p "p q r: x" == p "p q r:x"
> p "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
> p "self < 0.0 ifTrue: [0.0 - self] ifFalse: [self]"
> p "w * ((x + y) z)"
> p "w * (x + y) z"
> p "Transcript\n \"...\" show: 'hello ';\n show: 'world';\n cr."
> p "Point new setX: 25 setY: 35; isZero"
> p "self ifNil:" -- error
-}
basicExpression :: P BasicExpression
basicExpression = do
  p <- primary
  m <- P.optionMaybe messages
  c <- cascadedMessages
  return (BasicExpression p m (if null c then Nothing else Just c))

{- | <assignment target> := identifier

> p = stParse assignmentTarget
> p "self" -- ? ; error ; reserved words cannot be assigned to
-}
assignmentTarget :: P Identifier
assignmentTarget = P.label identifier "assignmentTarget"

-- | 3.4.5.2 (Expressions)
data Primary
  = PrimaryIdentifier Identifier
  | PrimaryLiteral Literal
  | PrimaryBlock BlockBody
  | PrimaryExpression Expression
  | PrimaryArrayExpression [BasicExpression] -- NON-ANSI
  deriving (Eq, Show)

-- | 3.4.5.3 Messages
data Messages
  = MessagesUnary [UnaryMessage] (Maybe [BinaryMessage]) (Maybe KeywordMessage)
  | MessagesBinary [BinaryMessage] (Maybe KeywordMessage)
  | MessagesKeyword KeywordMessage
  deriving (Eq,Show)

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

> p = stParse unaryMessages
> p "abs"
> p "abs square"
> p "abs square +p" == p "abs square + p"
> p "abs square +p +q" == p "abs square + p + q"
> p "value +b value +c value" == p "value + b value + c value"
> p "abs square +p +q k:r" == p "abs square + p + q k: r"
> p "abs square +p +q k1:p1 k2:p2" == p "abs square + p + q k1: p1 k2: p2"
> p "q r:x" == p "q r: x"
-}
unaryMessages :: P Messages
unaryMessages = do
  u <- P.many1 (P.try unaryMessage)
  b <- P.many binaryMessage
  k <- P.optionMaybe keywordMessage
  return (MessagesUnary u (if null b then Nothing else Just b) k)

-- | <binary message>+ [<keyword message>]
binaryMessages :: P Messages
binaryMessages = do
  b <- P.many1 binaryMessage
  k <- P.optionMaybe keywordMessage
  return (MessagesBinary b k)

{- | <messages> ::= (<unary message>+ <binary message>* [<keyword message>] )
                  | (<binary message>+ [<keyword message>] )
                  | <keyword message>

> rw = messages_pp . stParse messages
> (rw "k1:p1",rw "k1:p1 k2:p2")
> (rw "+p",rw "+p +q")
> (rw "+p k:r",rw "+p +q k:r")

> p = stParse messages
> p "k1:p1" == p "k1: p1"
> p "k1: p1 k2: p2" == p "k1:p1 k2:p2"
> p "+1"== p "+ 1"
> p "+p" == p "+ p"
> p "+p +q" == p "+ p + q"
> p "+p +q k:r" == p "+ p + q k: r"
> p "q r: x" == p "q r:x"
> p "< 0 ifTrue: [0 - self] ifFalse: [self]" == p "<0ifTrue:[0-self]ifFalse:[self]"
> p "x perform: #y: with: z" -- error
-}
messages :: P Messages
messages = P.choice [P.try binaryMessages,P.try (fmap MessagesKeyword keywordMessage),unaryMessages]

data UnaryMessage = UnaryMessage Identifier deriving (Eq,Show)

unaryMessageSelector :: UnaryMessage -> Selector
unaryMessageSelector (UnaryMessage u) = UnarySelector u

data BinaryMessage =
  BinaryMessage Identifier BinaryArgument
  deriving (Eq,Show)

binaryMessageSelector :: BinaryMessage -> Selector
binaryMessageSelector (BinaryMessage b _) = BinarySelector b

data KeywordMessage =
  KeywordMessage [(Keyword,KeywordArgument)]
  deriving (Eq,Show)

-- | Keyword selector from KeywordMessage.
keywordMessageSelector :: KeywordMessage -> Selector
keywordMessageSelector (KeywordMessage l) = KeywordSelector (concatMap fst l)

{- | <unary message> ::= unarySelector

> stParse unaryMessage "p" == UnaryMessage "p"
> stParse unaryMessage "" -- FAIL
-}
unaryMessage :: P UnaryMessage
unaryMessage = fmap (UnaryMessage . selectorIdentifier) unarySelector -- lexeme

{- | <binary message> ::= binarySelector <binary argument>

> stParse binaryMessage "+p" == stParse binaryMessage "+ p"
> stParse binaryMessage "+1" == stParse binaryMessage "+ 1"
> stParse binaryMessage "+1.0" == stParse binaryMessage "+ 1.0"
> stParse binaryMessage "+ 1 k:" -- + 1
-}
binaryMessage :: P BinaryMessage
binaryMessage = do
  BinarySelector sel <- binarySelector
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

> p = stParse keywordMessage
> p "k:p" == p "k: p"
> p "k1:p1 k2:p2" == p "k1: p1 k2: p2"
> p "k1: #p1:"
> p "k1: #p1: k2: #p2"
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

> p = stParse keywordArgument
> p "p"
> p "1"
> p "p q"
> p "p q r"
> p "p q r + s"
> p "p q r + s * t"
> p "#m: w:" == p "#m:"
> p "p k:" == p "p"
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

{- | <literal> ::= <number literal>
                 | <string literal>
                 | <character literal>
                 | <symbol literal>
                 | <selector literal>
                 | <array literal>
-}
data Literal
  = NumberLiteral Number
  | StringLiteral String
  | CharacterLiteral Char
  | SymbolLiteral Symbol
  | SelectorLiteral Selector
  | ArrayLiteral [Either Literal Identifier]
  deriving (Eq, Show)

{- | Parse literal.

> p = stParse literal
> p "123"
> p "-123"
> p "123.456"
> p "-123.456"
> p "'x'" == CharacterLiteral 'x'
> p "$x" == StringLiteral "x"
> p "#'xyz'" == SymbolLiteral "xyz"
> p "#abs" == SelectorLiteral (UnarySelector "abs")
> p "#m:" == SelectorLiteral (KeywordSelector "m:")
> p "#freq:iphase:" == SelectorLiteral (KeywordSelector "freq:iphase:")
> p "#+" == SelectorLiteral (BinarySelector "+")
> p "#(1 2.0 'x' $x #'xyz' #abs #freq:iphase: #+)"
> p "#(-12 -7 -5 0 2 5)"
> p "#(x y z)"
-}
literal :: P Literal
literal = P.choice [numberLiteral, stringLiteral, characterLiteral, P.try arrayLiteral, P.try symbolLiteral, selectorLiteral] -- lexeme

{- | <number literal> ::= ['-'] <number>

> stParse numberLiteral "123"
> stParse numberLiteral "123.456"
> stParse numberLiteral "-123"
> stParse numberLiteral "-123.456"
> stParse numberLiteral "1e-2"
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

-- | #( is the start token for literal arrays.
hashOpenParen :: P String
hashOpenParen = lexeme (P.string "#(")

closeParen :: P Char
closeParen = lexeme (P.char ')')

{- | 3.4.6.6
     <array literal> ::= '#(' <array element>* ')'

> p = stParse arrayLiteral
> p "#()" == p "#( )"
> p "#(1)" == p "#( 1 )"
> p "#(1 2.0)"
> p "#(1 2.0 3)"
> p "#(1 2.0 true)"
> p "#(1 #(2 3) 4)"
-}
arrayLiteral :: P Literal
arrayLiteral = fmap ArrayLiteral (P.between hashOpenParen closeParen (P.many arrayElement))

{- | <array element> ::= <literal> | identifier

If an identifier appears as an <array element> and it is one of the
reserved identifiers nil, true or false the value of the corresponding
element of the collection is the value of that reserved
identifier. The meaning is undefined if any other identifier is used
as an <array element>

> stParse arrayElement "1"
> stParse arrayElement "2.0"
> stParse arrayElement "nil"
-}
arrayElement :: P (Either Literal Identifier)
arrayElement = fmap Left literal P.<|> fmap Right reservedIdentifier -- lexeme

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

{- | letter ::= uppercaseAlphabetic
              | lowercaseAlphabetic
              | nonCaseLetter
              | "implementation defined letters"
-}
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

-- | C.f. 3.4.7
type ReservedIdentifier = Identifier

{- | One of stReservedIdentifiers

> p = stParse reservedIdentifier
> map p (words "self super nil")
> p "x" -- error
-}
reservedIdentifier :: P ReservedIdentifier
reservedIdentifier = lexeme (P.choice (map (P.try . P.string) stReservedIdentifiers))

type OrdinaryIdentifier = String

{- | An identifier that is not a reserved identifier

> p = stParse ordinaryIdentifier
> p "self" -- error
> p "x" == "x"
> p "x_" == "x_"
> p "x0" == "x0"
-}
ordinaryIdentifier :: P OrdinaryIdentifier
ordinaryIdentifier = P.identifier stLexer

{- | A non-lexeme variant of ordinaryIdentifier.  For keyword.

> p = stParse ordinaryIdentifierNonLexeme
> p "x" == "x"
> p "self" -- error
-}
ordinaryIdentifierNonLexeme :: P OrdinaryIdentifier
ordinaryIdentifierNonLexeme = do
  c0 <- P.letter
  cN <- P.many (P.letter P.<|> P.digit)
  let token = c0 : cN
  if token `elem` stReservedIdentifiers then P.unexpected "reserverdIdentifier" else return (c0 : cN)

type Identifier = String

{- | identifier ::= letter (letter | digit)*

> stParse identifier "x1" == "x1"
> stParse identifier "X1" == "X1"
> stParse identifier "1x" -- FAIL
> stParse identifier "" -- FAIL
> stParse identifier "true" == "true"
> stParse identifier "nil" == "nil"
> stParse identifier "y:" == "y"
> stParse identifier "#y:" -- error
-}
identifier :: P Identifier
identifier = ordinaryIdentifier P.<|> reservedIdentifier

-- * 3.5.4

{- | An identifier ending with a colon.

Keywords are identifiers followed immediately by the colon
character. An unadorned identifier is an identifier which is not
immediately preceded by a '#'. If a ':' followed by an '=' immediately
follows an unadorned identifier, with no intervening white space, then
the token is to be parsed as an identifier followed by an
assignmentOperator not as an keyword followed by an '='.
-}
type Keyword = Identifier

keywordNotLexeme :: P Keyword
keywordNotLexeme = do
  cs <- P.label ordinaryIdentifierNonLexeme "keyword"
  c <- P.label (P.char ':') "keyword"
  return (cs ++ [c])

{- | keyword ::= identifier ':'

> p = stParse keyword
> p "kw:" == "kw:"
> p "self:" -- error ?
> p "kw :" -- error
> p "kw" -- error
-}
keyword :: P Keyword
keyword = lexeme keywordNotLexeme

-- * 3.5.5

{- | binaryCharacter ::= '!' | '%' | '&' | '*' | '+' | ',' | '/'
                       | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'
-}
binaryCharacter :: P Char
binaryCharacter = P.oneOf "!%&*+,/<=>?@\\~|-"

-- | Identifier for binary operations.
type BinaryIdentifier = Identifier

-- | binarySelector ::= binaryCharacter+
--
-- > stParse binarySelector "+" == BinarySelector "+"
-- > stParse binarySelector "+p" == BinarySelector "+" -- +1 must parse as selector=+ argument=1
binarySelector :: P Selector
binarySelector = fmap BinarySelector (P.operator stLexer)

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

> p = stParse stringBody
> p "" == ""
> p "x" == "x"
> p "''" == "'" -- ?
> p "x''y" == "x'y"
> p "x'" == "x"
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

> stParse unarySelector "p" == UnarySelector "p"
> stParse unarySelector "p:" -- FAIL
-}
unarySelector :: P Selector
unarySelector =
  (identifier >>= \u -> P.notFollowedBy (P.char ':') >> return (UnarySelector u))
  P.<?> "unarySelector"

data Selector
  = UnarySelector Identifier
  | BinarySelector BinaryIdentifier
  | KeywordSelector Identifier
  deriving (Eq, Show)

-- | Identifier of Selector
selectorIdentifier :: Selector -> Identifier
selectorIdentifier s =
  case s of
    UnarySelector x -> x
    BinarySelector x -> x
    KeywordSelector x -> x

{- | Split KeywordSelector into it's components.

> keywordSelectorElements "freq:" == ["freq:"]
> keywordSelectorElements "freq:phase:" == ["freq:","phase:"]
> keywordSelectorElements "" == []
-}
keywordSelectorElements :: Identifier -> [Identifier]
keywordSelectorElements = takeWhile (not . null) . (split . keepDelimsR . onSublist) ":"

{- | Determine arity of selector

> map selectorArity [UnarySelector "abs",BinarySelector "+",KeywordSelector "at:put:"] == [0,1,2]
-}
selectorArity :: Selector -> Int
selectorArity s =
  case s of
    UnarySelector _ -> 0
    BinarySelector _ -> 1
    KeywordSelector x -> length (filter (== ':') x)

{- | quotedSelector ::= '#' (unarySelector | binarySelector | keywordSelector)

> p = stParse quotedSelector
> p "#abs" == UnarySelector "abs"
> p "#freq:" == KeywordSelector "freq:"
> p "#freq:iphase:" == KeywordSelector "freq:iphase:"
> p "#+" == BinarySelector "+"
> p "#+:" == p "#+" -- ?
> p "#+x" == p "#+" -- ?
> p "#'class'" -- error
-}
quotedSelector :: P Selector
quotedSelector =
  let sel = P.choice [binarySelector, P.try keywordSelector, unarySelector]
  in P.label (P.char '#' >> sel) "quotedSelector" -- lexeme

{- | keywordSelector ::= keyword+

> p = stParse keywordSelector
> p "freq:" == KeywordSelector "freq:"
> p "freq:iphase:" == KeywordSelector "freq:iphase:"
> p "p: q:" == KeywordSelector "p:"
> p "freq:iphase:x" -- error
-}
keywordSelector :: P Selector
keywordSelector = fmap (KeywordSelector . concat) (P.many1 keywordNotLexeme) P.<?> "keywordSelector"

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
