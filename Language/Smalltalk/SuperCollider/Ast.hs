{- | An abstract syntax tree (Ast) for SuperCollider (Sc).

This follows the structure of the ANSI Smalltalk (St) Ast.

ScTemporaries (3.4.2)
ScBlockBody (3.4.4)
ScStatements ScReturnStatement ScExpression ScBasicExpression ScPrimary ScMessages (3.4.5)

The most important differences are:

- in Sc Unary and Keyword messages have the same syntax and equal precedence
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

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

-- | Identifier with perhaps an initializer expression.
type ScTemporary = (St.Identifier,Maybe ScBasicExpression)

-- | 3.4.2. Sequence of temporaries, single var statement.
type ScTemporaries = [ScTemporary]

-- | 3.4.3
data ScInitializerDefinition =
  ScInitializerDefinition (Maybe [ScTemporaries]) (Maybe ScStatements)
  deriving (Eq,Show)

-- | 3.4.4
data ScBlockBody =
  ScBlockBody (Maybe [St.BlockArgument]) (Maybe [ScTemporaries]) (Maybe ScStatements)
  deriving (Eq,Show)

-- | 3.4.5
data ScStatements
  = ScStatementsReturn ScReturnStatement
  | ScStatementsExpression ScExpression (Maybe ScStatements)
  deriving (Eq,Show)

-- | 3.4.5.1
data ScReturnStatement =
  ScReturnStatement ScExpression
  deriving (Eq,Show)

-- | Prepend a list of expressions, as statements, to an existing statement.
scExpressionSequenceToStatements :: Maybe ScStatements -> [ScExpression] -> ScStatements
scExpressionSequenceToStatements stm =
  let f e =
        case e of
          [] -> error "scExpressionSequenceToStatements"
          [e0] -> ScStatementsExpression e0 stm
          e0:eN -> ScStatementsExpression e0 (Just (f eN))
  in f

-- | 3.4.5.2 Expressions
data ScExpression =
    ScExprAssignment St.Identifier ScExpression
  | ScExprBasic ScBasicExpression
  deriving (Eq, Show)

-- | 3.4.5.2
data ScBasicExpression =
    ScBasicExpression ScPrimary (Maybe ScMessages)
  deriving (Eq, Show)

{- | If the expression consists only of a primary, return that.
     If the expression has messages make a PrimaryExpression node.
-}
scBasicExpressionToPrimary :: ScBasicExpression -> ScPrimary
scBasicExpressionToPrimary e =
  case e of
    ScBasicExpression p Nothing -> p
    _ -> ScPrimaryExpression (ScExprBasic e)

{- | 3.4.5.2 Reuse the Smalltalk Literal type.
     The Sc notation "x(...)" is an implicit message send.
-}
data ScPrimary
  = ScPrimaryIdentifier St.Identifier
  | ScPrimaryLiteral St.Literal
  | ScPrimaryBlock ScBlockBody
  | ScPrimaryExpression ScExpression
  | ScPrimaryArrayExpression [ScBasicExpression]
  | ScPrimaryImplictMessageSend St.Identifier [ScBasicExpression]
  deriving (Eq, Show)

-- | 3.4.5.3
data ScMessages
  = ScMessagesDot [ScDotMessage] (Maybe [ScBinaryMessage])
  | ScMessagesBinary [ScBinaryMessage]
  deriving (Eq,Show)

{- | Sc allows x.y() although this is a kind of nonsense.
     Here we allow x.y as a unary message and x.y(z...) for n-ary messages where n >= 1.
     Ie. an empty parameter list here indicates a unary message.
-}
data ScDotMessage =
  ScDotMessage St.Identifier [ScKeywordArgument]
  deriving (Eq, Show)

-- | Does message have parameters, i.e. written as .q()
scDotMessageIsKeyword :: ScDotMessage -> Bool
scDotMessageIsKeyword (ScDotMessage _ m) = not (null m)

-- | Are any messages in the sequence keyword messages.
scDotMessagesHaveKeyword :: [ScDotMessage] -> Bool
scDotMessagesHaveKeyword = any scDotMessageIsKeyword

data ScBinaryMessage =
  ScBinaryMessage St.BinaryIdentifier ScBinaryArgument
  deriving (Eq, Show)

data ScBinaryArgument =
  ScBinaryArgument ScPrimary (Maybe [ScDotMessage])
  deriving (Eq, Show)

data ScKeywordArgument =
  ScKeywordArgument (Maybe St.Keyword) ScBasicExpression
  deriving (Eq, Show)
