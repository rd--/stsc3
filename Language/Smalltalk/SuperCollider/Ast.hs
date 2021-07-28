{- | An abstract syntax tree (Ast) for SuperCollider (Sc).

This is similar to the ANSI Smalltalk (St) Ast.

The most important differences are:

- in Sc Unary and Keyword messages have the same syntax and equal precedence
- in Sc Temporaries can introduce bindings
- in Sc all Keyword messages take one argument, which is a Dictionary
- in Sc if y is a unary message, "x.y" and "x.y()" and "x.y(nil)"

In St "x y" sends y to x, in Sc this is written "x.y".

In St "x y: a" sends the message (y:,a) to x, in Sc this is written "x.y (a)".

In St "(x y: a) z" sends (y:,a) to x and sends z to the result, in Sc this is written "x.y(a).z".

The Sc expression "x.y(a).z.y(b) + c" would be written "(((x y: a) z) y: b) + c" in St.
-}
module Language.Smalltalk.SuperCollider.Ast where

import Data.Maybe {- maybe -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}

-- | Reuse the Smalltalk Literal type.
data ScPrimary
  = ScPrimaryIdentifier St.Identifier
  | ScPrimaryLiteral St.Literal
  | ScPrimaryBlock ScBlockBody
  | ScPrimaryExpression ScExpression
  | ScPrimaryArrayExpression [ScBasicExpression]
  deriving (Eq, Show)

data ScBlockBody =
  ScBlockBody (Maybe [St.BlockArgument]) (Maybe [ScTemporaries]) (Maybe ScStatements)
  deriving (Eq,Show)

data ScStatements
  = ScStatementsReturn ScReturnStatement
  | ScStatementsExpression ScExpression (Maybe ScStatements)
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

data ScReturnStatement =
  ScReturnStatement ScExpression
  deriving (Eq,Show)

-- | Identifier with perhaps an initializer expression.
type ScTemporary = (St.Identifier,Maybe ScBasicExpression)

-- | Sequence of temporaries, single var statement.
type ScTemporaries = [ScTemporary]

data ScBasicExpression =
    ScBasicExpression ScPrimary (Maybe ScMessages)
  deriving (Eq, Show)

data ScMessages
  = ScMessagesDot [ScDotMessage] (Maybe [ScBinaryMessage])
  | ScMessagesBinary [ScBinaryMessage]
  deriving (Eq,Show)

-- | For translation we want to distinguish between x.y and x.y(),
--   so that x.abs does not send abs: #().
data ScDotMessage =
  ScDotMessage St.Identifier (Maybe [ScKeywordArgument])
  deriving (Eq, Show)

scDotMessageIsKeyword :: ScDotMessage -> Bool
scDotMessageIsKeyword (ScDotMessage _ m) = isJust m

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

data ScExpression =
    ScExprAssignment St.Identifier ScExpression
  | ScExprBasic ScBasicExpression
  deriving (Eq, Show)
