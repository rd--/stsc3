{- | An abstract syntax tree (Ast) for SuperCollider (Sc).

This Ast initially was for Sc, however it is now for C-Smalltalk (Stc).

This follows the structure of the ANSI Smalltalk (St) Ast.

ScTemporaries (3.4.2)
ScBlockBody (3.4.4)
ScStatements ScReturnStatement ScExpression ScBasicExpression ScPrimary ScMessages (3.4.5)

The most important differences are:

- in Sc Unary and Nary messages have the same syntax and equal precedence
- in Sc Temporaries can introduce bindings
- in Sc Block parameters are passed as a Dictionary and allow default values
- in Sc it is not possible to infer the arity of a Block from it's call site
  - in all of the following x may have any arity
  - .x .x() .x(nil) .x(y:nil) .x(y,z)

In St "x y" sends y to x, in Sc this is written "x.y".

In St "x y: a" sends the message y:a to x, in Sc this is written "x.y(a)".

In St "(x y: a) z" sends y:a to x and sends z to the result, in Sc this is written "x.y(a).z".

The Sc expression "x.y(a).z.y(b) + c" would be written "(((x y: a) z) y: b) + c" in St.
-}
module Language.Smalltalk.SuperCollider.Ast where

import Data.List {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

-- | Identifier with perhaps an initializer expression.
type ScTemporary = (St.LowercaseIdentifier, Maybe ScBasicExpression)

-- | Comments are text strings.
type ScComment = String

-- | Variable with optional default value, which must be a literal.
type ScVariable = (St.LowercaseIdentifier, Maybe St.Literal)

{- | 3.3.2

The parser for class definitions is partial.
Class and instance variables must be written in one sequence, without qualifiers.

In Sc class and instance variables may indicate that getter and setter methods are to be automatically generated.
These instructions would be given in the Ast as booleans attached to the variable name (implicitGetter, implicitSetter, variableName).
-}
data ScClassDefinition = ScClassDefinition
  { className :: St.UppercaseIdentifier
  , superclassName :: Maybe St.UppercaseIdentifier
  , classInstanceVariableNames :: Maybe [ScVariable]
  , classVariableNames :: Maybe [ScVariable]
  , methods :: [ScMethodDefinition]
  , classCategory :: Maybe String
  , classComment :: Maybe ScComment
  }
  deriving (Eq, Show)

-- | Partition methods into (class-methods, instance-methods).
scClassDefinitionPartitionMethods :: ScClassDefinition -> ([ScMethodDefinition], [ScMethodDefinition])
scClassDefinitionPartitionMethods = partition isClassMethod . methods

-- | Lookup names instance method.
scClassDefinitionLookupInstanceMethod :: ScClassDefinition -> St.LowercaseIdentifier -> Maybe ScMethodDefinition
scClassDefinitionLookupInstanceMethod cd nm = find (\m -> not (isClassMethod m) && methodName m == nm) (methods cd)

data ScClassExtension = ScClassExtension
  { extendClass :: St.UppercaseIdentifier
  , withMethods :: [ScMethodDefinition]
  }
  deriving (Eq, Show)

-- | 3.4.2
data ScMethodDefinition = ScMethodDefinition
  { isClassMethod :: Bool
  , methodName :: St.LowercaseIdentifier
  , methodBody :: ScBlockBody
  , methodCategory :: Maybe String
  , methodComment :: Maybe ScComment
  }
  deriving (Eq, Show)

-- | 3.4.2. Sequence of temporaries, single var statement.
type ScTemporaries = [ScTemporary]

-- | 3.4.3
data ScInitializerDefinition
  = ScInitializerDefinition (Maybe ScComment) (Maybe [ScTemporaries]) (Maybe ScStatements)
  deriving (Eq, Show)

-- | Set comment field
scInitializerDefinitionSetComment :: ScComment -> ScInitializerDefinition -> ScInitializerDefinition
scInitializerDefinitionSetComment c (ScInitializerDefinition _ t s) = ScInitializerDefinition (Just c) t s

-- | 3.4.4
data ScBlockBody = ScBlockBody
  { blockArguments :: Maybe [ScBlockArgument]
  , blockTemporaries :: Maybe [ScTemporaries]
  , blockStatements :: Maybe ScStatements
  }
  deriving (Eq, Show)

type ScBlockArgument = ScVariable

-- | 3.4.5
data ScStatements
  = ScStatementsReturn ScReturnStatement
  | ScStatementsExpression ScExpression (Maybe ScStatements)
  deriving (Eq, Show)

-- | 3.4.5.1
data ScReturnStatement
  = ScReturnStatement ScExpression
  deriving (Eq, Show)

-- | Prepend a list of expressions, as statements, to an existing statement.
scExpressionSequenceToStatements :: Maybe ScStatements -> [ScExpression] -> ScStatements
scExpressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "scExpressionSequenceToStatements"
          [e0] -> ScStatementsExpression e0 stm
          e0 : eN -> ScStatementsExpression e0 (Just (f eN))
  in f

-- | 3.4.5.2 Expressions
data ScExpression
  = ScExprAssignment St.LowercaseIdentifier ScExpression
  | ScExprBasic ScBasicExpression
  deriving (Eq, Show)

scPrimaryToBasicExpression :: ScPrimary -> ScBasicExpression
scPrimaryToBasicExpression p = ScBasicExpression p Nothing

-- | 3.4.5.2
data ScBasicExpression
  = ScBasicExpression ScPrimary (Maybe ScMessages)
  deriving (Eq, Show)

{- | If the expression consists only of a primary, return that.
     If the expression has messages make a PrimaryExpression node.
-}
scBasicExpressionToPrimary :: ScBasicExpression -> ScPrimary
scBasicExpressionToPrimary e =
  case e of
    ScBasicExpression p Nothing -> p
    _ -> ScPrimaryExpression (ScExprBasic e)

-- | If the expression is an ScBasicExpression then scBasicExpressionToPrimary, else ScPrimaryExpression.
scExpressionToPrimary :: ScExpression -> ScPrimary
scExpressionToPrimary e =
  case e of
    ScExprBasic b -> scBasicExpressionToPrimary b
    _ -> ScPrimaryExpression e

scIdentifierToBasicExpression :: St.Identifier -> ScBasicExpression
scIdentifierToBasicExpression i = ScBasicExpression (ScPrimaryIdentifier i) Nothing

scLiteralToBasicExpression :: St.Literal -> ScBasicExpression
scLiteralToBasicExpression l = ScBasicExpression (ScPrimaryLiteral l) Nothing

scIdentifierToExpression :: St.Identifier -> ScExpression
scIdentifierToExpression = ScExprBasic . scIdentifierToBasicExpression

{- | Constructor for dot message.

> scConstructDotMessage "at:" [ScBasicExpression (ScPrimaryIdentifier "key") Nothing]
-}
scConstructDotMessage :: St.LowercaseIdentifier -> [ScBasicExpression] -> ScMessages
scConstructDotMessage selector arguments = ScMessagesDot [ScDotMessage selector arguments] Nothing

scConstructDotMessageSend :: ScPrimary -> St.LowercaseIdentifier -> [ScBasicExpression] -> ScBasicExpression
scConstructDotMessageSend receiver selector arguments =
  ScBasicExpression receiver (Just (scConstructDotMessage selector arguments))

scDictionaryToBasicExpression :: [(St.LowercaseIdentifier, ScBasicExpression)] -> ScBasicExpression
scDictionaryToBasicExpression associationsArray =
  let f (key, value) = [scIdentifierToBasicExpression key, value]
      arrayExpression = ScPrimaryArrayExpression (concatMap f associationsArray)
  in scConstructDotMessageSend (ScPrimaryIdentifier "Dictionary") "newFromPairs" [ScBasicExpression arrayExpression Nothing]

scPrimaryKeywordMessageSend :: ScPrimary -> [(St.LowercaseIdentifier, ScBasicExpression)] -> ScPrimary
scPrimaryKeywordMessageSend receiver parameters =
  let selector = intercalate ":" (map fst parameters)
      arguments = map snd parameters
  in scBasicExpressionToPrimary (scConstructDotMessageSend receiver selector arguments)

scIntervalRange :: ScExpression -> ScExpression -> ScPrimary
scIntervalRange from to =
  scPrimaryKeywordMessageSend
    (ScPrimaryExpression from)
    [("to", ScBasicExpression (ScPrimaryExpression to) Nothing)]

scFromThenTo :: ScExpression -> ScExpression -> ScExpression -> ScPrimary
scFromThenTo from andThen to =
  scPrimaryKeywordMessageSend
    (ScPrimaryExpression from)
    [ ("then", ScBasicExpression (ScPrimaryExpression andThen) Nothing)
    , ("to", ScBasicExpression (ScPrimaryExpression to) Nothing)
    ]

scArrayRange :: ScExpression -> ScExpression -> ScPrimary
scArrayRange from to = scBasicExpressionToPrimary (scConstructDotMessageSend (scIntervalRange from to) "asArray" [])

{- | 3.4.5.2 Reuse the Smalltalk Literal type.
     The Sc notation "x(...)" is an implicit message send.
-}
data ScPrimary
  = ScPrimaryIdentifier St.Identifier
  | ScPrimaryLiteral St.Literal
  | ScPrimaryBlock ScBlockBody
  | ScPrimaryExpression ScExpression
  | ScPrimaryArrayExpression [ScBasicExpression]
  | ScPrimaryDictionaryExpression [(St.LowercaseIdentifier, ScBasicExpression)]
  | ScPrimaryImplicitMessageSend St.Identifier [ScBasicExpression]
  deriving (Eq, Show)

-- | 3.4.5.3
data ScMessages
  = ScMessagesDot [ScDotMessage] (Maybe [ScBinaryMessage])
  | ScMessagesBinary [ScBinaryMessage]
  deriving (Eq, Show)

{- | Sc allows x.y() although this is a kind of nonsense.
     Here we allow x.y as a unary message and x.y(z...) for n-ary messages where n >= 1.
     Ie. an empty parameter list here indicates a unary message.
-}
data ScDotMessage
  = ScDotMessage St.LowercaseIdentifier [ScBasicExpression]
  deriving (Eq, Show)

-- | Does message have parameters, i.e. written as .q()
scDotMessageIsNary :: ScDotMessage -> Bool
scDotMessageIsNary (ScDotMessage _ m) = not (null m)

-- | Are any messages in the sequence n-ary messages.
scDotMessagesHaveNary :: [ScDotMessage] -> Bool
scDotMessagesHaveNary = any scDotMessageIsNary

scDotMessageFromKeywordParam :: St.LowercaseIdentifier -> (ScBasicExpression, [(St.LowercaseIdentifier, ScBasicExpression)]) -> ScDotMessage
scDotMessageFromKeywordParam initialSelector (initialParam, keywordParam) =
  let selector = concat [initialSelector, ":", concatMap fst keywordParam]
      param = initialParam : map snd keywordParam
  in ScDotMessage selector param

data ScBinaryMessage
  = ScBinaryMessage (St.BinaryIdentifier, Maybe St.Identifier) ScBinaryArgument
  deriving (Eq, Show)

data ScBinaryArgument
  = ScBinaryArgument ScPrimary (Maybe [ScDotMessage])
  deriving (Eq, Show)

-- | List of Sc pseudo variables.  In addition to the St set it has, pi and inf.
scPseudoVariables :: [St.LowercaseIdentifier]
scPseudoVariables = words "nil true false inf pi self super"
