{- | An abstract syntax tree (Ast) for Spl.

This follows the structure of the ANSI Smalltalk (St) Ast.

SplTemporaries (3.4.2)
SplBlockBody (3.4.4)
SplStatements SplReturnStatement SplExpression SplBasicExpression SplPrimary SplMessages (3.4.5)

The most important differences are:

- in Spl Unary and Nary messages have the same syntax and equal precedence
- in Spl Temporaries can introduce bindings

In St "x y" sends y to x, in Spl this is written "x.y".

In St "x y: a" sends the message y:a to x, in Spl this is written "x.y(a)".

In St "(x y: a) z" sends y:a to x and sends z to the result, in Spl this is written "x.y(a).z".

The Spl expression "x.y(a).z.y(b) + c" would be written "(((x y: a) z) y: b) + c" in St.
-}
module Language.Smalltalk.Spl.Ast where

import Data.List {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

-- | Identifier with perhaps an initializer expression.
type SplTemporary = (St.LowercaseIdentifier, Maybe SplBasicExpression)

-- | Comments are text strings.
type SplComment = String

-- | Variable with optional default value, which must be a literal.
type SplVariable = (St.LowercaseIdentifier, Maybe St.Literal)

{- | 3.3.2

The parser for class definitions is partial.
Class and instance variables must be written in one sequence, without qualifiers.

In Sc class and instance variables may indicate that getter and setter methods are to be automatically generated.
These instructions would be given in the Ast as booleans attached to the variable name (implicitGetter, implicitSetter, variableName).
-}
data SplClassDefinition = SplClassDefinition
  { className :: St.UppercaseIdentifier
  , superclassName :: Maybe St.UppercaseIdentifier
  , classInstanceVariableNames :: Maybe [SplVariable]
  , classVariableNames :: Maybe [SplVariable]
  , methods :: [SplMethodDefinition]
  , classCategory :: Maybe String
  , classComment :: Maybe SplComment
  }
  deriving (Eq, Show)

-- | Partition methods into (class-methods, instance-methods).
splClassDefinitionPartitionMethods :: SplClassDefinition -> ([SplMethodDefinition], [SplMethodDefinition])
splClassDefinitionPartitionMethods = partition isClassMethod . methods

-- | Lookup names instance method.
splClassDefinitionLookupInstanceMethod :: SplClassDefinition -> St.LowercaseIdentifier -> Maybe SplMethodDefinition
splClassDefinitionLookupInstanceMethod cd nm = find (\m -> not (isClassMethod m) && methodName m == nm) (methods cd)

data SplClassExtension = SplClassExtension
  { extendClass :: St.UppercaseIdentifier
  , withMethods :: [SplMethodDefinition]
  }
  deriving (Eq, Show)

-- | 3.4.2
data SplMethodDefinition = SplMethodDefinition
  { isClassMethod :: Bool
  , methodName :: St.LowercaseIdentifier
  , methodBody :: SplBlockBody
  , methodCategory :: Maybe String
  , methodComment :: Maybe SplComment
  }
  deriving (Eq, Show)

-- | 3.4.2. Sequence of temporaries, single var statement.
type SplTemporaries = [SplTemporary]

-- | 3.4.3
data SplInitializerDefinition
  = SplInitializerDefinition (Maybe SplComment) (Maybe [SplTemporaries]) (Maybe SplStatements)
  deriving (Eq, Show)

-- | Set comment field
splInitializerDefinitionSetComment :: SplComment -> SplInitializerDefinition -> SplInitializerDefinition
splInitializerDefinitionSetComment c (SplInitializerDefinition _ t s) = SplInitializerDefinition (Just c) t s

-- | 3.4.4
data SplBlockBody = SplBlockBody
  { blockArguments :: Maybe [SplBlockArgument]
  , blockTemporaries :: Maybe [SplTemporaries]
  , blockStatements :: Maybe SplStatements
  }
  deriving (Eq, Show)

type SplBlockArgument = SplVariable

-- | 3.4.5
data SplStatements
  = SplStatementsReturn SplReturnStatement
  | SplStatementsExpression SplExpression (Maybe SplStatements)
  deriving (Eq, Show)

-- | 3.4.5.1
data SplReturnStatement
  = SplReturnStatement SplExpression
  deriving (Eq, Show)

-- | Prepend a list of expressions, as statements, to an existing statement.
splExpressionSequenceToStatements :: Maybe SplStatements -> [SplExpression] -> SplStatements
splExpressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "splExpressionSequenceToStatements"
          [e0] -> SplStatementsExpression e0 stm
          e0 : eN -> SplStatementsExpression e0 (Just (f eN))
  in f

-- | 3.4.5.2 Expressions
data SplExpression
  = SplExprAssignment St.LowercaseIdentifier SplExpression
  | SplExprBasic SplBasicExpression
  deriving (Eq, Show)

splPrimaryToBasicExpression :: SplPrimary -> SplBasicExpression
splPrimaryToBasicExpression p = SplBasicExpression p Nothing

splPrimaryToExpression :: SplPrimary -> SplExpression
splPrimaryToExpression p = SplExprBasic (splPrimaryToBasicExpression p)

-- | 3.4.5.2
data SplBasicExpression
  = SplBasicExpression SplPrimary (Maybe SplMessages)
  deriving (Eq, Show)

{- | If the expression consists only of a primary, return that.
     If the expression has messages make a PrimaryExpression node.
-}
splBasicExpressionToPrimary :: SplBasicExpression -> SplPrimary
splBasicExpressionToPrimary e =
  case e of
    SplBasicExpression p Nothing -> p
    _ -> SplPrimaryExpression (SplExprBasic e)

-- | If the expression is an SplBasicExpression then splBasicExpressionToPrimary, else SplPrimaryExpression.
splExpressionToPrimary :: SplExpression -> SplPrimary
splExpressionToPrimary e =
  case e of
    SplExprBasic b -> splBasicExpressionToPrimary b
    _ -> SplPrimaryExpression e

splIdentifierToBasicExpression :: St.Identifier -> SplBasicExpression
splIdentifierToBasicExpression i = SplBasicExpression (SplPrimaryIdentifier i) Nothing

splLiteralToBasicExpression :: St.Literal -> SplBasicExpression
splLiteralToBasicExpression l = SplBasicExpression (SplPrimaryLiteral l) Nothing

splIdentifierToExpression :: St.Identifier -> SplExpression
splIdentifierToExpression = SplExprBasic . splIdentifierToBasicExpression

{- | Constructor for dot message.

> splConstructDotMessage "at:" [SplBasicExpression (SplPrimaryIdentifier "key") Nothing]
-}
splConstructDotMessage :: St.LowercaseIdentifier -> [SplBasicExpression] -> SplMessages
splConstructDotMessage selector arguments = SplMessagesDot [SplDotMessage selector arguments] Nothing

splConstructDotMessageSend :: SplPrimary -> St.LowercaseIdentifier -> [SplBasicExpression] -> SplBasicExpression
splConstructDotMessageSend receiver selector arguments =
  SplBasicExpression receiver (Just (splConstructDotMessage selector arguments))

splDictionaryToBasicExpression :: [(St.LowercaseIdentifier, SplBasicExpression)] -> SplBasicExpression
splDictionaryToBasicExpression associationsArray =
  let f (key, value) = [splIdentifierToBasicExpression key, value]
      arrayExpression = SplPrimaryArrayExpression (concatMap f associationsArray)
  in splConstructDotMessageSend (SplPrimaryIdentifier "Dictionary") "newFromPairs" [SplBasicExpression arrayExpression Nothing]

splPrimaryKeywordMessageSend :: SplPrimary -> [(St.LowercaseIdentifier, SplBasicExpression)] -> SplPrimary
splPrimaryKeywordMessageSend receiver parameters =
  let selector = intercalate ":" (map fst parameters)
      arguments = map snd parameters
  in splBasicExpressionToPrimary (splConstructDotMessageSend receiver selector arguments)

splMethodCall :: SplExpression -> [St.LowercaseIdentifier] -> [SplExpression] -> SplPrimary
splMethodCall receiver messageParts arguments =
  splPrimaryKeywordMessageSend
    (SplPrimaryExpression receiver)
    (zip messageParts (map (\x -> SplBasicExpression (SplPrimaryExpression x) Nothing) arguments))

splIntervalRange :: SplExpression -> SplExpression -> SplPrimary
splIntervalRange from to = splMethodCall from ["to"] [to]

splFromThenTo :: SplExpression -> SplExpression -> SplExpression -> SplPrimary
splFromThenTo from andThen to = splMethodCall from ["then", "to"] [andThen, to]

splFromToBy :: SplExpression -> SplExpression -> SplExpression -> SplPrimary
splFromToBy from to by = splMethodCall from ["to", "by"] [to, by]

splArrayRange :: SplExpression -> SplExpression -> SplPrimary
splArrayRange from to = splBasicExpressionToPrimary (splConstructDotMessageSend (splIntervalRange from to) "asArray" [])

splArrayFromToBy :: SplExpression -> SplExpression -> SplExpression -> SplPrimary
splArrayFromToBy from to by = splBasicExpressionToPrimary (splConstructDotMessageSend (splFromToBy from to by) "asArray" [])

type SplDictionaryItems = [(St.LowercaseIdentifier, SplBasicExpression)]

{- | 3.4.5.2 Reuse the Smalltalk Literal type.
     The Sc notation "x(...)" is an implicit message send.
-}
data SplPrimary
  = SplPrimaryIdentifier St.Identifier
  | SplPrimaryLiteral St.Literal
  | SplPrimaryBlock SplBlockBody
  | SplPrimaryExpression SplExpression
  | SplPrimaryArrayExpression [SplBasicExpression]
  | SplPrimaryDictionaryExpression SplDictionaryItems
  | SplPrimaryImplicitMessageSend St.Identifier [SplBasicExpression]
  deriving (Eq, Show)

-- | 3.4.5.3
data SplMessages
  = SplMessagesDot [SplDotMessage] (Maybe [SplBinaryMessage])
  | SplMessagesBinary [SplBinaryMessage]
  deriving (Eq, Show)

{- | Sc allows x.y() although this is a kind of nonsense.
     Here we allow x.y as a unary message and x.y(z...) for n-ary messages where n >= 1.
     Ie. an empty parameter list here indicates a unary message.
-}
data SplDotMessage
  = SplDotMessage St.LowercaseIdentifier [SplBasicExpression]
  deriving (Eq, Show)

-- | Does message have parameters, i.e. written as .q()
splDotMessageIsNary :: SplDotMessage -> Bool
splDotMessageIsNary (SplDotMessage _ m) = not (null m)

-- | Are any messages in the sequence n-ary messages.
splDotMessagesHaveNary :: [SplDotMessage] -> Bool
splDotMessagesHaveNary = any splDotMessageIsNary

splDotMessageFromKeywordParam :: St.LowercaseIdentifier -> (SplBasicExpression, [(St.LowercaseIdentifier, SplBasicExpression)]) -> SplDotMessage
splDotMessageFromKeywordParam initialSelector (initialParam, keywordParam) =
  let selector = concat [initialSelector, ":", concatMap fst keywordParam]
      param = initialParam : map snd keywordParam
  in SplDotMessage selector param

data SplBinaryMessage
  = SplBinaryMessage (St.BinaryIdentifier, Maybe St.Identifier) SplBinaryArgument
  deriving (Eq, Show)

data SplBinaryArgument
  = SplBinaryArgument SplPrimary (Maybe [SplDotMessage])
  deriving (Eq, Show)

splMatrixExpression :: [[SplBasicExpression]] -> SplPrimary
splMatrixExpression =
  SplPrimaryArrayExpression
    . map (splPrimaryToBasicExpression . SplPrimaryArrayExpression)

splListConstructorSyntax :: St.UppercaseIdentifier -> SplPrimary -> SplPrimary
splListConstructorSyntax nm itm =
  SplPrimaryImplicitMessageSend
  nm
  [splPrimaryToBasicExpression itm]

splNegatedIdentifier :: St.LowercaseIdentifier -> SplPrimary
splNegatedIdentifier x =
  SplPrimaryImplicitMessageSend
  "negate"
  [splPrimaryToBasicExpression (SplPrimaryIdentifier (drop 1 x))]

splRecordConstructorSyntax :: St.Identifier -> SplDictionaryItems -> SplPrimary
splRecordConstructorSyntax nm itm =
  SplPrimaryImplicitMessageSend
  nm
  [splPrimaryToBasicExpression (SplPrimaryDictionaryExpression itm)]
