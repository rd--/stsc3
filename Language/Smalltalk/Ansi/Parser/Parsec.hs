-- | Parsec parser for a subset of ANSI Smalltalk.
module Language.Smalltalk.Ansi.Parser.Parsec where

import Control.Monad {- base -}
import Data.Char {- base -}
import qualified Data.Functor.Identity as Identity {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}

import qualified Text.Parsec as P {- parsec -}
import qualified Text.Parsec.Language as Language {- parsec -}
import qualified Text.Parsec.String as String {- parsec -}
import qualified Text.Parsec.Token as Token {- parsec -}

import Language.Smalltalk.Ansi.Ast {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Ast.Helper as Helper {- stsc3 -}

-- | A 'Char' parser with no user state.
type P a = String.GenParser Char () a

-- | Run p then q, returning result of p.
(>>~) :: Monad m => m t -> m u -> m t
p >>~ q = p >>= \x -> q >> return x

-- | /p/ consuming any trailing separators.
lexeme :: P t -> P t
lexeme = Token.lexeme stLexer

-- | Smalltalk language definition (for token parser)
stLanguageDef :: Language.GenLanguageDef String () Identity.Identity
stLanguageDef =
  Token.LanguageDef
    { Token.commentStart = "\""
    , Token.commentEnd = "\""
    , Token.commentLine = "" -- NIL
    , Token.nestedComments = False
    , Token.identStart = letter
    , Token.identLetter = letter P.<|> P.digit
    , Token.opStart = binaryCharacter
    , Token.opLetter = binaryCharacter
    , Token.reservedNames = Helper.stReservedIdentifiers
    , Token.reservedOpNames = []
    , Token.caseSensitive = True
    }

-- | Lexer
stLexer :: Token.GenTokenParser String () Identity.Identity
stLexer = Token.makeTokenParser stLanguageDef

-- | Run parser and return either an error string or an answer.
stParseEither :: P t -> String -> Either String t
stParseEither p = either (\m -> Left ("stParse: " ++ show m)) Right . P.parse p ""

-- | Run parser and report any error.  Does not delete leading spaces.
stParse :: P t -> String -> t
stParse p = either (\e -> error e) id . stParseEither p

-- | Run parser and report maybe give answer.
stParseMaybe :: P t -> String -> Maybe t
stParseMaybe p = either (\_ -> Nothing) Just . P.parse p ""

-- | Delete leading spaces and run stParse.
stParseInitial :: P t -> String -> t
stParseInitial p =
  let deleteLeadingSpaces = dropWhile isSpace
  in stParse p . deleteLeadingSpaces

-- * 3.3 Smalltalk Abstract Program Grammar

-- * 3.3.1 Program Definition

{- | <<Smalltalk program>> ::= <<program element>>+ <<initialization ordering>>

>>> p = stParse smalltalkProgram
>>> p "" == SmalltalkProgram {programElements = []}
True

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

{- | <<program element>> ::= <<class definition>>
                           | <<global definition>>
                           | <<pool definition>>
                           | <<program initializer definition>>
-}
programElement :: P ProgramElement
programElement =
  P.choice
    [ fmap ProgramInitializer programInitializerDefinition
    , fmap ProgramGlobal globalDefinition
    ]

nonEmptyProgramElement :: P ProgramElement
nonEmptyProgramElement =
  P.choice
    [ fmap ProgramInitializer nonEmptyProgramInitializerDefinition
    , fmap ProgramGlobal globalDefinition
    ]

-- * 3.3.2 Class Definition

-- * 3.3.3 Global Variable Definition

{- | <<global definition>> ::= [<<constant designator>>] <<global name>> [<<variable initializer>>]

>>> p = stParse globalDefinition
>>> p "g"
GlobalDefinition "g" (Just (InitializerDefinition Nothing Nothing Nothing))

> p "g g := 0"
-}
globalDefinition :: P GlobalDefinition
globalDefinition = do
  -- constant designator
  n <- identifier
  v <- P.optionMaybe initializerDefinition
  return (GlobalDefinition n v)

-- * 3.3.5 Program Initializer Definition

{- | <<program initializer definition >> ::= <initializer definition>

>>> p = stParse programInitializerDefinition

> p "|t| t + 1"
> p "\"x\" |t| t + 1"

>>> p ""
InitializerDefinition Nothing Nothing Nothing
-}
programInitializerDefinition :: P ProgramInitializerDefinition
programInitializerDefinition = initializerDefinition

-- | A variant that does will not match the empty string.
nonEmptyProgramInitializerDefinition :: P ProgramInitializerDefinition
nonEmptyProgramInitializerDefinition = nonEmptyInitializerDefinition

-- * 3.4.2

duplicateNamesError :: [LowercaseIdentifier] -> P ()
duplicateNamesError dup = when (not (null dup)) (P.unexpected ("Name already used: " ++ show dup))

{- | <method definition> ::= <message pattern> [<temporaries>] [<statements>]

> p = stParse (methodDefinition Nothing ("", False))
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
> p "p \"c\" ^q"
> p "p |tmp| <primitive: 0> self continueAfterPrimitive"
> p "p |tmp| <primitive: 'stringAsLabel'> ^nil"
> p "p |tmp| <primitive: identifierAsLabel> ^nil"
> p "p <primitive: 0> |tmp| self primitiveFailed" -- this should fail, instead it discards statements!
> p "p: q | q | ^q" -- duplicate name error
-}
methodDefinition :: Maybe String -> (UppercaseIdentifier, Bool) -> P MethodDefinition
methodDefinition src cl = do
  pat <- messagePattern -- messagePattern is a token and consumes trailing comments
  cmt <- P.optionMaybe comment -- this is always Nothing
  tmp <- P.optionMaybe temporaries
  prm <- P.optionMaybe primitive -- primitive comes after temporaries
  stm <- P.optionMaybe statements
  let def = MethodDefinition cl Nothing pat tmp stm prm cmt src
      dup = Helper.methodDefinitionDuplicateTemporaries def
  duplicateNamesError dup
  return def

{- | <message pattern> ::= <unary pattern> | <binary pattern> | <keyword pattern>

>>> let p = stParse messagePattern

>>> p "p"
UnaryPattern "p"

>>> p "+p" == p "+ p"
True

>>> p "k1:p1" == p "k1: p1"
True

>>> p "k1:p1 k2:p2" == p "k1: p1 k2: p2"
True

>>> p "k: v x" == p "k:v"
True
-}
messagePattern :: P Pattern
messagePattern =
  P.choice
    [ P.try keywordPattern
    , P.try binaryPattern
    , unaryPattern
    ]
    P.<?> "messagePattern"

{- | <unary pattern> ::= unarySelector

>>> stParse unaryPattern "p"
UnaryPattern "p"

> stParse unaryPattern "p:" -- error
-}
unaryPattern :: P Pattern
unaryPattern = fmap (UnaryPattern . Helper.selectorIdentifier) unarySelector -- lexeme

{- | <binary pattern> ::= binarySelector <method argument>

>>> stParse binaryPattern "+p" == stParse binaryPattern "+ p"
True
-}
binaryPattern :: P Pattern
binaryPattern = do
  BinarySelector sel <- binarySelector
  arg <- identifier
  return (BinaryPattern sel arg)

{- | <keyword pattern> ::= (keyword <method argument>)+

>>> stParse keywordPattern "k: p"
KeywordPattern [("k:","p")]

>>> stParse keywordPattern "k1: p1 k2: p2"
KeywordPattern [("k1:","p1"),("k2:","p2")]

>>> stParse keywordPattern "k1: p1 ..."
KeywordPattern [("k1:","p1")]
-}
keywordPattern :: P Pattern
keywordPattern = do
  let f = do
        kw <- keyword
        arg <- identifier
        return (kw, arg)
  fmap KeywordPattern (P.many1 (P.try f))

-- | Vertical bar as lexeme (token).
verticalBar :: P Char
verticalBar = lexeme (P.char '|')

temporariesIdentifierSequence :: P [LowercaseIdentifier]
temporariesIdentifierSequence = P.between verticalBar verticalBar temporary_variable_list

{- | <temporaries> ::= '|' <temporary variable list> '|'

>>> let p = stParse temporaries
>>> p "||"
Temporaries {temporariesIdentifiers = []}

>>> p "|p|" == p "| p|"
True

>>> p "|p |" == p "| p |"
True

>>> p "|p q r|"
Temporaries {temporariesIdentifiers = ["p","q","r"]}
-}
temporaries :: P Temporaries
temporaries = P.label (fmap Temporaries temporariesIdentifierSequence) "temporaries"

{- | <temporary variable list> ::= identifier*

>>> let p = stParse temporary_variable_list
>>> p ""
[]

>>> p "p"
["p"]

>>> p "p q"
["p","q"]

>>> p "p q r" == p "p q r +"
True
-}
temporary_variable_list :: P [LowercaseIdentifier]
temporary_variable_list = P.many identifier P.<?> "temporary_variable_list"

-- * 3.4.3

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
  return (InitializerDefinition Nothing t s)

nonEmptyInitializerDefinition :: P InitializerDefinition
nonEmptyInitializerDefinition = do
  InitializerDefinition c t s <- initializerDefinition
  case (t, s) of
    (Nothing, Nothing) -> P.unexpected "nonEmptyInitializerDefinition: empty"
    _ -> return (InitializerDefinition c t s)

-- * 3.4.4

{- | <block constructor> ::= '[' <block body> ']'

> p = stParse blockConstructor
> p "[1]" == p "[ 1 ]"
> p "[^1]" ==  p "[ ^ 1 .]"
> p "[:a| |t|]" == p "[ :a | | t | ]"
> p "[:a| |t u| x . y]"
> p "[:a| |t u| t := [:b| |v w| z] . x . y]"
> p "[:ignored]"
-}
blockConstructor :: P BlockBody
blockConstructor = inBrackets blockBody

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
> p ":ignored"
> p ":a| | a | a + 1" -- duplicate name error
-}
blockBody :: P BlockBody
blockBody = do
  a <- P.optionMaybe (P.try (P.many1 blockArgument >>~ verticalBar))
  t <- P.optionMaybe temporaries
  s <- P.optionMaybe statements
  let blk = BlockBody Nothing a t s
      dup = Helper.blockBodyDuplicateTemporaries blk
  duplicateNamesError dup
  return blk

{- | <block argument> ::= ':' identifier

> p = stParse blockArgument
> p ":a" == p ": a"
-}
blockArgument :: P BlockArgument
blockArgument = lexeme (P.char ':') >> identifier

-- * 3.4.5

{- | '.' as lexeme (token)

> stParse period "." == "."
-}
period :: P String
period = Token.dot stLexer

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
  P.choice [fmap StatementsReturn (returnStatement >>~ P.optional period), rhs]

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
> p "Point new x: 25 y: 35; isZero"
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

inParentheses :: P t -> P t
inParentheses = Token.parens stLexer

inBraces :: P t -> P t
inBraces = Token.braces stLexer

inBrackets :: P t -> P t
inBrackets = Token.brackets stLexer

{- | <primary> ::= identifier | <literal> | <block constructor> | ( '(' <expression> ')' )

> let p = stParse primary
> p "p" == PrimaryIdentifier "p"
> p "1"
> p "[:a| a + 1]"
> p "(p)"
> p "(p q)"
> p "(p q: r)"
> p "(1 + 2)"
> map p ["{p.q.r}", "{p. q. r}", "{ p . q . r }", "{ p . q . r . }", "{p.q.r.}"]
> map p ["{p. {q. r} }", "{p. {q. r.} .}"]
-}
primary :: P Primary
primary =
  lexeme
    ( P.choice
        [ fmap PrimaryBlock blockConstructor
        , fmap PrimaryExpression (inParentheses expression)
        , fmap PrimaryArrayExpression (inBraces (P.sepEndBy basicExpression period)) -- allow ending period?
        , fmap PrimaryIdentifier identifier
        , fmap PrimaryLiteral literal
        ]
        P.<?> "primary"
    )

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
messages = P.choice [P.try binaryMessages, P.try (fmap MessagesKeyword keywordMessage), unaryMessages]

{- | <unary message> ::= unarySelector

> stParse unaryMessage "p" == UnaryMessage "p"
> stParse unaryMessage "" -- FAIL
-}
unaryMessage :: P UnaryMessage
unaryMessage = fmap (UnaryMessage . Helper.selectorIdentifier) unarySelector -- lexeme

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
  let f = do
        kw <- keyword
        arg <- keywordArgument
        return (kw, arg)
  fmap KeywordMessage (P.many1 f)

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
cascadeSeparator = Token.semi stLexer

-- | <cascaded messages> ::= (';' <messages>)*
cascadedMessages :: P CascadedMessages
cascadedMessages = P.many (cascadeSeparator >> messages)

-- * 3.4.6

{- | Parse literal.

>>> p = stParse literal
>>> p "123"
NumberLiteral (Int 123)

>>> p "-123"
NumberLiteral (Int (-123))

>>> p "123.456"
NumberLiteral (Float 123.456)

>>> p "-123.456"
NumberLiteral (Float (-123.456))

>>> p "'x'"
StringLiteral "x"

>>> p "$x"
CharacterLiteral 'x'

>>> p "#'xyz'"
SymbolLiteral "xyz"

>>> p "#abs"
SelectorLiteral (UnarySelector "abs")

>>> p "#m:"
SelectorLiteral (KeywordSelector "m:" 1)

>>> p "#freq:iphase:"
SelectorLiteral (KeywordSelector "freq:iphase:" 2)

>>> p "#+"
SelectorLiteral (BinarySelector "+")

> p "#(1 2.0 'x') $x #'xyz' #abs #freq:iphase: #+)"

> p "#(-12 -7 -5 0 2 5)"

> p "#(x y: #z: 1)"

> p "x"
-}
literal :: P Literal
literal =
  P.choice
    [ numberLiteral
    , stringLiteral
    , characterLiteral
    , P.try arrayLiteral
    , P.try symbolLiteral
    , selectorLiteral -- lexeme
    ]

{- | <number literal> ::= ['-'] <number>

>>> stParse numberLiteral "123"
NumberLiteral (Int 123)

>>> stParse numberLiteral "123.456"
NumberLiteral (Float 123.456)

>>> stParse numberLiteral "-123"
NumberLiteral (Int (-123))

>>> stParse numberLiteral "-123.456"
NumberLiteral (Float (-123.456))

>>> stParse numberLiteral "1e-2"
NumberLiteral (Float 1.0e-2)
-}
numberLiteral :: P Literal
numberLiteral = do
  n <- P.optionMaybe (P.char '-')
  let rw :: Num n => n -> n
      rw = maybe id (const negate) n
  fmap (NumberLiteral . Helper.numberCase (Int . rw) (Float . rw)) number -- lexeme

{- | <number> ::= integer | float | scaledDecimal

>>> map (stParse number) (words "1 1.2")
[Int 1,Float 1.2]

> map (stParse number) (words "-1 -1.2") -- FAIL
-}
number :: P Number
number = fmap Float (P.try float) P.<|> fmap Int integer -- lexeme

-- | <character literal> ::= quotedCharacter
characterLiteral :: P Literal
characterLiteral = fmap CharacterLiteral quotedCharacter

{- | <string literal> ::= quotedString

>>> let p = stParse stringLiteral
>>> p "'x'"
StringLiteral "x"

>>> p "'\\n'"
StringLiteral "\\n"
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

openParen :: P Char
openParen = lexeme (P.char '(')

closeParen :: P Char
closeParen = lexeme (P.char ')')

{- | 3.4.6.6
     <array literal> ::= '#(' <array element>* ')'

>>> let p = stParse arrayLiteral
>>> p "#()" == p "#( )"
True

>>> p "#(1)" == p "#( 1 )"
True

>>> p "#(1 2.0)"
ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0))]

>>> p "#(1 2.0 3)"
ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0)),Left (NumberLiteral (Int 3))]

>>> p "#(1 2.0 true)"
ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0)),Right "true"]

>>> p "#(1 #(2 3) 4)" == p "#(1 (2 3) 4)"
True

>>> p "#(x x: #x: nil)" == p "#(#'x' #'x:' #x: nil)"
True
-}
arrayLiteral :: P Literal
arrayLiteral = fmap ArrayLiteral (P.between hashOpenParen closeParen (P.many arrayElement))

{- | Allow array elements within literal arrays to elide the # at the opening parenthesis.
      This is the rule in the ST-80 sources.
      Non-Ansi.
-}
interiorArrayLiteral :: P Literal
interiorArrayLiteral = fmap ArrayLiteral (P.between openParen closeParen (P.many arrayElement))

{- | Allow identifiers (both ordinary and keyword) within literal arrays to elide the #.
      This is the rule in the ST-80 sources.
      Non-Ansi.

> map (stParse interiorSymbol) (words "x y: #z:")
-}
interiorSymbol :: P Literal
interiorSymbol = fmap SymbolLiteral (P.try keyword P.<|> identifier)

{- | <array element> ::= <literal> | identifier

Ansi:
If an identifier appears as an <array element> and it is one of the
reserved identifiers nil, true or false the value of the corresponding
element of the collection is the value of that reserved
identifier. The meaning is undefined if any other identifier is used
as an <array element>

ST-80:
The Ansi rule is not the rule from ST-80, where #(x x: nil) means #(#'x' #'y:' nil).

>>> let p = stParse arrayElement
>>> p "1"
Left (NumberLiteral (Int 1))

>>> p "2.0"
Left (NumberLiteral (Float 2.0))

>>> p "nil"
Right "nil"

>>> p "(1 2.0 nil symbol keyword: #hashedKeyword: -1)"
Left (ArrayLiteral [Left (NumberLiteral (Int 1)),Left (NumberLiteral (Float 2.0)),Right "nil",Left (SymbolLiteral "symbol"),Left (SymbolLiteral "keyword:"),Left (SelectorLiteral (KeywordSelector "hashedKeyword:" 1)),Left (NumberLiteral (Int (-1)))])

>>> p "x"
Left (SymbolLiteral "x")

>>> p "x:"
Left (SymbolLiteral "x:")

>>> p "#x:"
Left (SelectorLiteral (KeywordSelector "x:" 1))

>>> stParse (P.many arrayElement) "1 '2' 3.14 x #'y' -1 #z: -2"
[Left (NumberLiteral (Int 1)),Left (StringLiteral "2"),Left (NumberLiteral (Float 3.14)),Left (SymbolLiteral "x"),Left (SymbolLiteral "y"),Left (NumberLiteral (Int (-1))),Left (SelectorLiteral (KeywordSelector "z:" 1)),Left (NumberLiteral (Int (-2)))]
-}
arrayElement :: P (Either Literal Identifier)
arrayElement = fmap Right reservedIdentifier P.<|> fmap Left (literal P.<|> interiorArrayLiteral P.<|> interiorSymbol) -- lexeme

-- * 3.4.7 Reserved Identifiers

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

>>> stParse nonCommentDelimiter "x"
'x'

> stParse nonCommentDelimiter "\"" -- FAIL
-}
nonCommentDelimiter :: P Char
nonCommentDelimiter = P.noneOf ['"']

{- | comment := commentDelimiter nonCommentDelimiter * commentDelimiter

>>> stParse comment "\"\""
""

>>> stParse comment "\" x\""
" x"

>>> stParse comment "\"x \""
"x "

>>> stParse comment "\"analog bubbles (jmcc) #1\""
"analog bubbles (jmcc) #1"
-}
comment :: P String
comment = P.between commentDelimiter (lexeme commentDelimiter) (P.many nonCommentDelimiter)

-- * 3.5.3

{- | One of stReservedIdentifiers

>>> let p = stParse reservedIdentifier
>>> map p (words "self super nil")
["self","super","nil"]

> p "x" -- error
-}
reservedIdentifier :: P ReservedIdentifier
reservedIdentifier = lexeme (P.choice (map (P.try . P.string) Helper.stReservedIdentifiers))

{- | An identifier that is not a reserved identifier

>>> let p = stParse ordinaryIdentifier
>>> p "x"
"x"

>>> p "x_"
"x_"

>>> p "x0"
"x0"

> p "self" -- error
-}
ordinaryIdentifier :: P OrdinaryIdentifier
ordinaryIdentifier = Token.identifier stLexer

--  if token `elem` stReservedIdentifiers then P.unexpected ("reservedIdentifier: " ++ token) else return token

underscore :: P Char
underscore = P.char '_'

{- | A non-lexeme variant of ordinaryIdentifier.  For keyword.

>>> let p = stParse identifierNonLexemeForKeyword
>>> p "x"
"x"

>>> p "x_1"
"x_1"

>>> p "super" -- super: as part of a keyword is allowed, &etc.
"super"
-}
identifierNonLexemeForKeyword :: P OrdinaryIdentifier
identifierNonLexemeForKeyword = do
  c0 <- P.letter
  cN <- P.many (P.letter P.<|> underscore P.<|> P.digit)
  return (c0 : cN)

{- | identifier ::= letter (letter | digit)*

Ansi doesn't allow underscore but Squeak does, also some Sc Ugens have underscores in bot the class (PV_) and parameter names.

>>> let p = stParse identifier
>>> p "x1"
"x1"

>>> p "X1"
"X1"

>>> p "x_1"
"x_1"

> p "1x" -- FAIL

> p "" -- FAIL

>>> p "true"
"true"

>>> p "nil"
"nil"

>>> p "y:"
"y"

> p "#y:" -- error
-}
identifier :: P Identifier
identifier = ordinaryIdentifier P.<|> reservedIdentifier

-- * 3.5.4

-- | A keyword parser that is not a lexeme.  A keyword selector is a sequence of non-lexeme keywords.
keywordNotLexeme :: P Keyword
keywordNotLexeme = do
  cs <- P.label identifierNonLexemeForKeyword "keyword"
  c <- P.label (P.char ':') "keyword"
  return (cs ++ [c])

{- | keyword ::= identifier ':'

>>> let p = stParse keyword
>>> p "kw:"
"kw:"

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
binaryCharacter = P.oneOf Helper.binaryCharacterSet

{- | binarySelector ::= binaryCharacter+

>>> stParse binarySelector "&"
BinarySelector "&"

>>> stParse binarySelector "+p" -- +1 must parse as selector=+ argument=1
BinarySelector "+"
-}
binarySelector :: P Selector
binarySelector = fmap BinarySelector (Token.operator stLexer)

-- | returnOperator ::= '^'
returnOperator :: P Char
returnOperator = lexeme (P.char '^')

-- | assignmentOperator ::= ':='
assignmentOperator :: P String
assignmentOperator = lexeme (P.string ":=")

-- * 3.5.6

{- | integer ::= decimalInteger | radixInteger

>>> let p = stParse integer
>>> map p (words "63 8r77 99 10r99 255 16rFF")
[63,63,99,99,255,255]
-}
integer :: P Integer
integer = P.try radixInteger P.<|> decimalInteger

{- | radixInteger ::= radixSpecifier 'r' radixDigits

>>> let p = stParse radixInteger
>>> map p (words "8r77 10r99 16rFF")
[63,99,255]

>>> map p (words "2r10111 8r27 10r23 16r17")
[23,23,23,23]
-}
radixInteger :: P Integer
radixInteger = fmap (fromMaybe (error "radixInteger?")) radixIntegerMaybe

radixIntegerMaybe :: P (Maybe Integer)
radixIntegerMaybe = do
  rs <- radixSpecifier
  _ <- P.char 'r'
  rd <- lexeme radixDigits
  let get x =
        case x of
          [(answer, "")] -> Just answer
          _ -> Nothing
      n =
        case rs of
          2 -> get (readBin rd) -- base=4.16.1
          8 -> get (readOct rd)
          10 -> get (readDec rd)
          16 -> get (readHex rd)
          _ -> Nothing
  return n

-- | radixSpecifier := digits
radixSpecifier :: P Integer
radixSpecifier = fmap read digits

-- | radixDigits ::= (digit | uppercaseAlphabetic)+
radixDigits :: P String
radixDigits = P.many1 (digit P.<|> uppercaseAlphabetic)

-- | decimalInteger ::= digits
decimalInteger :: P Integer
decimalInteger = fmap read digits

-- | digits ::= digit+
digits :: P String
digits = lexeme (P.many1 P.digit)

-- | float ::= mantissa [exponentLetter exponent]
float :: P Double
float = Token.float stLexer

-- * 3.5.7

{- | quotedCharacter ::= '$' character

>>> stParse quotedCharacter "$x"
'x'
-}
quotedCharacter :: P Char
quotedCharacter = P.label (P.char '$' >> lexeme P.anyChar) "quotedCharacter"

-- * 3.5.8

{- | quotedString ::= stringDelimiter stringBody stringDelimiter

>>> let p = stParse quotedString
>>> p "''"
""

>>> p "''''"
"'"

>>> p "' xy'"
" xy"

>>> p "'x''y'"
"x'y"

>>> p "'''x''y'''"
"'x'y'"

>>> p "'\n'"
"\n"

>>> p "'x\n'"
"x\n"

>>> p "'\0x\n'"
"\NULx\n"

>>> p "'\\n'"
"\\n"
-}
quotedString :: P QuotedString
quotedString = P.between stringDelimiter (lexeme stringDelimiter) stringBody -- lexeme

{- | stringBody ::= (nonStringDelimiter | (stringDelimiter stringDelimiter)*)

>>> let p = stParse stringBody
>>> p ""
""

>>> p "x y"
"x y"

>>> p "x"
"x"

>>> p "x''"
"x'"

>>> p "x'"
"x"
-}
stringBody :: P String
stringBody = P.many (P.try escapedStringDelimiter P.<|> nonStringDelimiter)

-- | stringDelimiter ::= ''' "a single quote"
stringDelimiter :: P Char
stringDelimiter = P.char '\'' -- non-lexeme (the lhs is not a lexeme, the rhs is)

-- | nonStringDelimiter ::= "any character except stringDelimiter"
nonStringDelimiter :: P Char
nonStringDelimiter = P.noneOf "'"

-- | Ansi escaped quote (quote quote).
escapedStringDelimiter :: P Char
escapedStringDelimiter = stringDelimiter >> stringDelimiter

-- * 3.5.9

{- | hashedString ::= '#' quotedString

>>> stParse hashedString "#'x'"
"x"
-}
hashedString :: P HashedString
hashedString = (P.char '#' >> quotedString) P.<?> "hashedString"

-- * 3.5.10

{- | Identifier (lexeme), not ending with ':'

>>> stParse unarySelector "p"
UnarySelector "p"

> stParse unarySelector "p:" -- FAIL
-}
unarySelector :: P Selector
unarySelector =
  (identifier >>= \u -> P.notFollowedBy (P.char ':') >> return (UnarySelector u))
    P.<?> "unarySelector"

{- | quotedSelector ::= '#' (unarySelector | binarySelector | keywordSelector)

>>> let p = stParse quotedSelector
>>> p "#abs"
UnarySelector "abs"

>>> p "#freq:"
KeywordSelector "freq:" 1

>>> p "#freq:iphase:"
KeywordSelector "freq:iphase:" 2

>>> p "#+"
BinarySelector "+"

>>> p "#+:" == p "#+" -- ?
True

>>> p "#+x" == p "#+" -- ?
True

> p "#'class'" -- error
-}
quotedSelector :: P Selector
quotedSelector =
  let sel = P.choice [binarySelector, P.try keywordSelector, unarySelector]
  in P.label (P.char '#' >> sel) "quotedSelector" -- lexeme

{- | keywordSelector ::= keyword+

>>> let p = stParse keywordSelector
>>> p "freq:"
KeywordSelector "freq:" 1

>>> p "freq:iphase:"
KeywordSelector "freq:iphase:" 2

>>> p "p: q:"
KeywordSelector "p:" 1

> p "freq:iphase:x" -- error
-}
keywordSelectorNotLexeme :: P Selector
keywordSelectorNotLexeme =
  fmap (Helper.asKeywordSelector . concat) (P.many1 keywordNotLexeme) P.<?> "keywordSelector"

keywordSelector :: P Selector
keywordSelector = lexeme keywordSelectorNotLexeme

-- * 3.5.11

{- | separator ::= (whitespace | comment)*

>>> stParse separator " "
" "

>>> stParse separator "\t"
"\t"

>>> stParse separator "\n"
"\n"

>>> stParse separator "  "
"  "

>>> stParse separator " \t"
" \t"

>>> stParse separator "\"commentary\""
"commentary"

>>> stParse separator "\t\"commentary\""
"\tcommentary"

> stParse separator "" -- FAIL
-}
separator :: P String
separator = fmap concat (P.many1 (fmap return P.space P.<|> comment) P.<?> "separator")

-- * Primitive

primitive :: P Primitive
primitive = do
  _ <- lexeme (P.char '<')
  _ <- lexeme (P.string "primitive")
  _ <- lexeme (P.char ':')
  l <- literal P.<|> interiorSymbol
  _ <- lexeme (P.char '>')
  return (Primitive l)
