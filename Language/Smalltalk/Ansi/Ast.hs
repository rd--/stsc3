{- | Ast for a subset of ANSI Smalltalk.
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
module Language.Smalltalk.Ansi.Ast where

import qualified Data.Graph as Graph {- containers -}

-- * 3.3.1 Program Definition

-- | Sequence of program elements.
data SmalltalkProgram = SmalltalkProgram {programElements :: [ProgramElement]}
  deriving (Eq, Show)

-- | 3.3.1
data ProgramElement
  = ProgramGlobal GlobalDefinition
  | ProgramInitializer ProgramInitializerDefinition
  deriving (Eq, Show)

-- * 3.3.2 Class Definition

data Indexable = ByteIndexable | NonIndexable | ObjectIndexable | WordIndexable deriving (Eq, Show)

data InstanceState = InstanceState Indexable [Identifier] deriving (Eq, Show)

{- | Smalltalk has conventions regarding identifier case, but not strict rules.
In .stc it would be nice to allow p.Q to mean Q(p), ie. distinguishing between q and Q.
The types here are used to indicate where there is an ordinary case for a particular identifier.
-}
type LowercaseIdentifier = Identifier

type UppercaseIdentifier = Identifier

{- | A class definition defines the behavior and encapsulated state of objects.

superclassName is empty (Nothing) if the class is the base class (i.e. Object or ProtoObject) else it is the name of the super class.
An empty superclassName does not indicate that a "default" superclass should be selected.
-}
data ClassDefinition = ClassDefinition
  { className :: UppercaseIdentifier
  , superclassName :: Maybe UppercaseIdentifier
  , instanceState :: InstanceState
  , classInstanceVariableNames :: [LowercaseIdentifier]
  , classVariableNames :: [UppercaseIdentifier]
  , importedPoolNames :: [UppercaseIdentifier]
  , instanceMethods :: [MethodDefinition]
  , classMethods :: [MethodDefinition]
  , classInitializer :: Maybe InitializerDefinition
  , classCategory :: Maybe String -- Meta-data
  , classComment :: Maybe String -- Meta-data
  }
  deriving (Eq, Show)

-- | A class library is a collection of class definitions.
type ClassLibrary = [ClassDefinition]

-- | (Major, Minor)
type ClassCategoryParts = (String, String)

type ClassDefinitionGraph =
  ( Graph.Graph
  , Graph.Vertex -> (ClassDefinition, UppercaseIdentifier, [UppercaseIdentifier])
  , UppercaseIdentifier -> Maybe Graph.Vertex
  )

-- * 3.3.3 Global Variable Definition

-- | 3.3.3
data GlobalDefinition
  = GlobalDefinition GlobalName (Maybe VariableInitializer)
  deriving (Eq, Show)

-- | <<global name>> ::= identifier
type GlobalName = UppercaseIdentifier

-- | <<variable initializer>> ::= <initializer definition>
type VariableInitializer = InitializerDefinition

-- * 3.3.5 Program Initializer Definition

-- | 3.3.5
type ProgramInitializerDefinition = InitializerDefinition

-- * 3.4.2

-- | Method category (Meta-data)
type MethodCategory = String

{- | 3.4.2

     It is a convention that method comments, if they exist, are written directly after the pattern.
     The parser presently does not store Method comments, however there is a placeholder field.
     In order to store comments:
       - stLanguageDef should not define comment start and end
       - there should be a comment preserving lexeme form, lexemeWithMaybeComment
       - methodPattern should be such a lexeme
     There is a field for storing the method source text.
     The methodClass field indicates which class the method is held by.
     It will be ("Name", False) for instance methods and ("Name class", True) for class methods.
-}
data MethodDefinition = MethodDefinition
  { methodClass :: (UppercaseIdentifier, Bool)
  -- ^ Holder
  , methodCategory :: Maybe MethodCategory
  -- ^ Meta-data
  , methodPattern :: Pattern
  , methodTemporaries :: Maybe Temporaries
  , methodStatements :: Maybe Statements
  , methodPrimitive :: Maybe Primitive
  -- ^ Non-ansi
  , methodComment :: Maybe String
  , methodSource :: Maybe String
  }
  deriving (Eq, Show)

-- | (isClassSide, selector)
type MethodDescriptor = (Bool, Selector)

data Pattern
  = UnaryPattern LowercaseIdentifier
  | BinaryPattern BinaryIdentifier LowercaseIdentifier
  | KeywordPattern [(Keyword, LowercaseIdentifier)]
  deriving (Eq, Show)

-- | A method name is a (className,methodSignature)
type MethodName = (UppercaseIdentifier, LowercaseIdentifier)

-- | 3.4.2
data Temporaries = Temporaries {temporariesIdentifiers :: [LowercaseIdentifier]} deriving (Eq, Show)

-- * 3.4.3

-- | Comments are text strings
type Comment = String

{- | 3.4.3

The value of an initializer with no <statements> is the binding of the reserved identifier 'nil'.
-}
data InitializerDefinition
  = InitializerDefinition (Maybe Comment) (Maybe Temporaries) (Maybe Statements)
  deriving (Eq, Show)

-- * 3.4.4

{- | A block has optional arguments, optional temporaries and optional statements.
     The method name field is not assigned by the parser, see methodDefinitionAnnotateBlocks.
-}
data BlockBody = BlockBody
  { blockMethodName :: Maybe MethodName
  -- ^ Meta data
  , blockArguments :: Maybe [BlockArgument]
  , blockTemporaries :: Maybe Temporaries
  , blockStatements :: Maybe Statements
  }
  deriving (Eq, Show)

-- | An identifier for a block argument.  Written with a ':' prefix.
type BlockArgument = LowercaseIdentifier

-- * 3.4.5

-- | 3.4.5
data Statements
  = StatementsReturn ReturnStatement
  | StatementsExpression Expression (Maybe Statements)
  deriving (Eq, Show)

-- | 3.4.5.1 Return statement
data ReturnStatement = ReturnStatement Expression deriving (Eq, Show)

-- | 3.4.5.2 Expressions
data Expression = ExprAssignment Assignment | ExprBasic BasicExpression deriving (Eq, Show)

{- | 3.4.5.2 (Expressions)
Class variables may be assigned to, and they are upper case.
-}
data Assignment = Assignment Identifier Expression deriving (Eq, Show)

-- | 3.4.5.2
data BasicExpression
  = BasicExpression Primary (Maybe Messages) (Maybe CascadedMessages)
  deriving (Eq, Show)

-- | 3.4.5.2 (Expressions)
data Primary
  = PrimaryIdentifier Identifier
  | PrimaryLiteral Literal
  | PrimaryBlock BlockBody
  | PrimaryExpression Expression
  | PrimaryArrayExpression [BasicExpression] -- non-Ansi
  deriving (Eq, Show)

-- | 3.4.5.3 Messages
data Messages
  = MessagesUnary [UnaryMessage] (Maybe [BinaryMessage]) (Maybe KeywordMessage)
  | MessagesBinary [BinaryMessage] (Maybe KeywordMessage)
  | MessagesKeyword KeywordMessage
  deriving (Eq, Show)

data UnaryMessage = UnaryMessage LowercaseIdentifier deriving (Eq, Show)

data BinaryMessage
  = BinaryMessage BinaryIdentifier BinaryArgument
  deriving (Eq, Show)

data KeywordMessage
  = KeywordMessage [(Keyword, KeywordArgument)]
  deriving (Eq, Show)

data BinaryArgument
  = BinaryArgument Primary (Maybe [UnaryMessage])
  deriving (Eq, Show)

data KeywordArgument
  = KeywordArgument Primary (Maybe [UnaryMessage]) (Maybe [BinaryMessage])
  deriving (Eq, Show)

type CascadedMessages = [Messages]

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

data Number = Int Integer | Float Double deriving (Eq, Show)

-- * 3.4.7 Reserved Identifiers

-- * 3.5.1

-- * 3.5.2

-- * 3.5.3

-- | C.f. 3.4.7
type ReservedIdentifier = Identifier

type OrdinaryIdentifier = String

type Identifier = String

-- * 3.5.4

{- | An identifier ending with a colon.

Keywords are identifiers followed immediately by the colon
character. An unadorned identifier is an identifier which is not
immediately preceded by a '#'. If a ':' followed by an '=' immediately
follows an unadorned identifier, with no intervening white space, then
the token is to be parsed as an identifier followed by an
assignmentOperator not as an keyword followed by an '='.
-}
type Keyword = LowercaseIdentifier

-- * 3.5.5

-- | Identifier for binary operations.
type BinaryIdentifier = Identifier

-- * 3.5.6

-- * 3.5.7

type QuotedCharacter = Char

-- * 3.5.8

-- | String quote character is '
type QuotedString = String

-- * 3.5.9

type HashedString = String

-- * 3.5.10

data Selector
  = UnarySelector LowercaseIdentifier
  | BinarySelector BinaryIdentifier
  | KeywordSelector LowercaseIdentifier Int
  deriving (Eq, Ord, Show)

-- * 3.5.11

-- * Primitive

{- | Parse Squeak/Gnu type Vm primitive. (Non-Ansi).
Allow unquoted symbol, which will be printed back unquoted (as would a quoted symbol).

>>> stParse primitive "<primitive: 63>"
Primitive {primitiveLabel = NumberLiteral (Int 63)}

>>> stParse primitive "<primitive: VMpr_ByteString_at>"
Primitive {primitiveLabel = SymbolLiteral "VMpr_ByteString_at"}
-}
data Primitive = Primitive {primitiveLabel :: Literal} deriving (Eq, Show)
