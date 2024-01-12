{- | An abstract syntax tree (Ast) for C-Smalltalk (Stc).

This follows the structure of the ANSI Smalltalk (St) Ast.

StcTemporaries (3.4.2)
StcBlockBody (3.4.4)
StcStatements StcReturnStatement StcExpression StcBasicExpression StcPrimary StcMessages (3.4.5)

The most important differences are:

- in Stc Unary and Nary messages have the same syntax and equal precedence
- in Stc Temporaries can introduce bindings

In St "x y" sends y to x, in Stc this is written "x.y".

In St "x y: a" sends the message y:a to x, in Stc this is written "x.y(a)".

In St "(x y: a) z" sends y:a to x and sends z to the result, in Stc this is written "x.y(a).z".

The Stc expression "x.y(a).z.y(b) + c" would be written "(((x y: a) z) y: b) + c" in St.
-}
module Language.Smalltalk.Stc.Ast where

import Data.List {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

-- | Identifier with perhaps an initializer expression.
type StcTemporary = (St.LowercaseIdentifier, Maybe StcBasicExpression)

-- | Comments are text strings.
type StcComment = String

-- | Variable with optional default value, which must be a literal.
type StcVariable = (St.LowercaseIdentifier, Maybe St.Literal)

{- | 3.3.2

The parser for class definitions is partial.
Class and instance variables must be written in one sequence, without qualifiers.

In Sc class and instance variables may indicate that getter and setter methods are to be automatically generated.
These instructions would be given in the Ast as booleans attached to the variable name (implicitGetter, implicitSetter, variableName).
-}
data StcClassDefinition = StcClassDefinition
  { className :: St.UppercaseIdentifier
  , superclassName :: Maybe St.UppercaseIdentifier
  , classInstanceVariableNames :: Maybe [StcVariable]
  , classVariableNames :: Maybe [StcVariable]
  , methods :: [StcMethodDefinition]
  , classCategory :: Maybe String
  , classComment :: Maybe StcComment
  }
  deriving (Eq, Show)

-- | Partition methods into (class-methods, instance-methods).
stcClassDefinitionPartitionMethods :: StcClassDefinition -> ([StcMethodDefinition], [StcMethodDefinition])
stcClassDefinitionPartitionMethods = partition isClassMethod . methods

-- | Lookup names instance method.
stcClassDefinitionLookupInstanceMethod :: StcClassDefinition -> St.LowercaseIdentifier -> Maybe StcMethodDefinition
stcClassDefinitionLookupInstanceMethod cd nm = find (\m -> not (isClassMethod m) && methodName m == nm) (methods cd)

data StcClassExtension = StcClassExtension
  { extendClass :: St.UppercaseIdentifier
  , withMethods :: [StcMethodDefinition]
  }
  deriving (Eq, Show)

-- | 3.4.2
data StcMethodDefinition = StcMethodDefinition
  { isClassMethod :: Bool
  , methodName :: St.LowercaseIdentifier
  , methodBody :: StcBlockBody
  , methodCategory :: Maybe String
  , methodComment :: Maybe StcComment
  }
  deriving (Eq, Show)

-- | 3.4.2. Sequence of temporaries, single var statement.
type StcTemporaries = [StcTemporary]

-- | 3.4.3
data StcInitializerDefinition
  = StcInitializerDefinition (Maybe StcComment) (Maybe [StcTemporaries]) (Maybe StcStatements)
  deriving (Eq, Show)

-- | Set comment field
stcInitializerDefinitionSetComment :: StcComment -> StcInitializerDefinition -> StcInitializerDefinition
stcInitializerDefinitionSetComment c (StcInitializerDefinition _ t s) = StcInitializerDefinition (Just c) t s

-- | 3.4.4
data StcBlockBody = StcBlockBody
  { blockArguments :: Maybe [StcBlockArgument]
  , blockTemporaries :: Maybe [StcTemporaries]
  , blockStatements :: Maybe StcStatements
  }
  deriving (Eq, Show)

type StcBlockArgument = StcVariable

-- | 3.4.5
data StcStatements
  = StcStatementsReturn StcReturnStatement
  | StcStatementsExpression StcExpression (Maybe StcStatements)
  deriving (Eq, Show)

-- | 3.4.5.1
data StcReturnStatement
  = StcReturnStatement StcExpression
  deriving (Eq, Show)

-- | Prepend a list of expressions, as statements, to an existing statement.
stcExpressionSequenceToStatements :: Maybe StcStatements -> [StcExpression] -> StcStatements
stcExpressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "stcExpressionSequenceToStatements"
          [e0] -> StcStatementsExpression e0 stm
          e0 : eN -> StcStatementsExpression e0 (Just (f eN))
  in f

-- | 3.4.5.2 Expressions
data StcExpression
  = StcExprAssignment St.LowercaseIdentifier StcExpression
  | StcExprBasic StcBasicExpression
  deriving (Eq, Show)

stcPrimaryToBasicExpression :: StcPrimary -> StcBasicExpression
stcPrimaryToBasicExpression p = StcBasicExpression p Nothing

-- | 3.4.5.2
data StcBasicExpression
  = StcBasicExpression StcPrimary (Maybe StcMessages)
  deriving (Eq, Show)

{- | If the expression consists only of a primary, return that.
     If the expression has messages make a PrimaryExpression node.
-}
stcBasicExpressionToPrimary :: StcBasicExpression -> StcPrimary
stcBasicExpressionToPrimary e =
  case e of
    StcBasicExpression p Nothing -> p
    _ -> StcPrimaryExpression (StcExprBasic e)

-- | If the expression is an StcBasicExpression then stcBasicExpressionToPrimary, else StcPrimaryExpression.
stcExpressionToPrimary :: StcExpression -> StcPrimary
stcExpressionToPrimary e =
  case e of
    StcExprBasic b -> stcBasicExpressionToPrimary b
    _ -> StcPrimaryExpression e

stcIdentifierToBasicExpression :: St.Identifier -> StcBasicExpression
stcIdentifierToBasicExpression i = StcBasicExpression (StcPrimaryIdentifier i) Nothing

stcLiteralToBasicExpression :: St.Literal -> StcBasicExpression
stcLiteralToBasicExpression l = StcBasicExpression (StcPrimaryLiteral l) Nothing

stcIdentifierToExpression :: St.Identifier -> StcExpression
stcIdentifierToExpression = StcExprBasic . stcIdentifierToBasicExpression

{- | Constructor for dot message.

> stcConstructDotMessage "at:" [StcBasicExpression (StcPrimaryIdentifier "key") Nothing]
-}
stcConstructDotMessage :: St.LowercaseIdentifier -> [StcBasicExpression] -> StcMessages
stcConstructDotMessage selector arguments = StcMessagesDot [StcDotMessage selector arguments] Nothing

stcConstructDotMessageSend :: StcPrimary -> St.LowercaseIdentifier -> [StcBasicExpression] -> StcBasicExpression
stcConstructDotMessageSend receiver selector arguments =
  StcBasicExpression receiver (Just (stcConstructDotMessage selector arguments))

stcDictionaryToBasicExpression :: [(St.LowercaseIdentifier, StcBasicExpression)] -> StcBasicExpression
stcDictionaryToBasicExpression associationsArray =
  let f (key, value) = [stcIdentifierToBasicExpression key, value]
      arrayExpression = StcPrimaryArrayExpression (concatMap f associationsArray)
  in stcConstructDotMessageSend (StcPrimaryIdentifier "Dictionary") "newFromPairs" [StcBasicExpression arrayExpression Nothing]

stcPrimaryKeywordMessageSend :: StcPrimary -> [(St.LowercaseIdentifier, StcBasicExpression)] -> StcPrimary
stcPrimaryKeywordMessageSend receiver parameters =
  let selector = intercalate ":" (map fst parameters)
      arguments = map snd parameters
  in stcBasicExpressionToPrimary (stcConstructDotMessageSend receiver selector arguments)

stcIntervalRange :: StcExpression -> StcExpression -> StcPrimary
stcIntervalRange from to =
  stcPrimaryKeywordMessageSend
    (StcPrimaryExpression from)
    [("to", StcBasicExpression (StcPrimaryExpression to) Nothing)]

stcFromThenTo :: StcExpression -> StcExpression -> StcExpression -> StcPrimary
stcFromThenTo from andThen to =
  stcPrimaryKeywordMessageSend
    (StcPrimaryExpression from)
    [ ("then", StcBasicExpression (StcPrimaryExpression andThen) Nothing)
    , ("to", StcBasicExpression (StcPrimaryExpression to) Nothing)
    ]

stcArrayRange :: StcExpression -> StcExpression -> StcPrimary
stcArrayRange from to = stcBasicExpressionToPrimary (stcConstructDotMessageSend (stcIntervalRange from to) "asArray" [])

{- | 3.4.5.2 Reuse the Smalltalk Literal type.
     The Sc notation "x(...)" is an implicit message send.
-}
data StcPrimary
  = StcPrimaryIdentifier St.Identifier
  | StcPrimaryLiteral St.Literal
  | StcPrimaryBlock StcBlockBody
  | StcPrimaryExpression StcExpression
  | StcPrimaryArrayExpression [StcBasicExpression]
  | StcPrimaryDictionaryExpression [(St.LowercaseIdentifier, StcBasicExpression)]
  | StcPrimaryImplicitMessageSend St.Identifier [StcBasicExpression]
  deriving (Eq, Show)

-- | 3.4.5.3
data StcMessages
  = StcMessagesDot [StcDotMessage] (Maybe [StcBinaryMessage])
  | StcMessagesBinary [StcBinaryMessage]
  deriving (Eq, Show)

{- | Sc allows x.y() although this is a kind of nonsense.
     Here we allow x.y as a unary message and x.y(z...) for n-ary messages where n >= 1.
     Ie. an empty parameter list here indicates a unary message.
-}
data StcDotMessage
  = StcDotMessage St.LowercaseIdentifier [StcBasicExpression]
  deriving (Eq, Show)

-- | Does message have parameters, i.e. written as .q()
stcDotMessageIsNary :: StcDotMessage -> Bool
stcDotMessageIsNary (StcDotMessage _ m) = not (null m)

-- | Are any messages in the sequence n-ary messages.
stcDotMessagesHaveNary :: [StcDotMessage] -> Bool
stcDotMessagesHaveNary = any stcDotMessageIsNary

stcDotMessageFromKeywordParam :: St.LowercaseIdentifier -> (StcBasicExpression, [(St.LowercaseIdentifier, StcBasicExpression)]) -> StcDotMessage
stcDotMessageFromKeywordParam initialSelector (initialParam, keywordParam) =
  let selector = concat [initialSelector, ":", concatMap fst keywordParam]
      param = initialParam : map snd keywordParam
  in StcDotMessage selector param

data StcBinaryMessage
  = StcBinaryMessage (St.BinaryIdentifier, Maybe St.Identifier) StcBinaryArgument
  deriving (Eq, Show)

data StcBinaryArgument
  = StcBinaryArgument StcPrimary (Maybe [StcDotMessage])
  deriving (Eq, Show)
