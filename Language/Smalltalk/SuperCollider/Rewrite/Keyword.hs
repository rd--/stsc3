{- | Keyword parameter re-writing.

     Rewrite Keyword messages to have a single association array as argument.
     ScDotMessage occurs in ScMessages and in ScBinaryArgument, which occurs in ScMessages.
     ScMessages occurs in ScBasicExpression.
-}
module Language.Smalltalk.SuperCollider.Rewrite.Keyword where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

-- | Translate ScKeywordArgument (k:v) to Association (k -> v)
scKeywordAssoc :: St.Keyword -> ScBasicExpression -> ScBasicExpression
scKeywordAssoc k v =
  let p = ScPrimaryLiteral (St.SelectorLiteral (St.KeywordSelector k))
      rhs = scBasicExpressionToPrimary v
      m = ScMessagesBinary [ScBinaryMessage "->" (ScBinaryArgument rhs Nothing)]
  in ScBasicExpression p (Just m)

-- | Rewrite value and, if ScKeywordArgument has a keyword, translate to association.
scKeywordArgumentAssoc :: ScKeywordArgument -> ScBasicExpression
scKeywordArgumentAssoc (ScKeywordArgument k v) =
  case k of
    Just x -> scKeywordAssoc x (scBasicExpressionRewriteKeyword v)
    Nothing -> scBasicExpressionRewriteKeyword v

-- | Translate ScKeywordArgument list into Sc Array.
scKeywordArgumentsArray :: [ScKeywordArgument] -> ScBasicExpression
scKeywordArgumentsArray a =
  ScBasicExpression (ScPrimaryArrayExpression (map scKeywordArgumentAssoc a)) Nothing

scKeywordArgumentsRewriteKeyword :: [ScKeywordArgument] -> [ScKeywordArgument]
scKeywordArgumentsRewriteKeyword p =
  [ScKeywordArgument Nothing (scKeywordArgumentsArray p)]

scDotMessageRewriteKeyword :: ScDotMessage -> ScDotMessage
scDotMessageRewriteKeyword (ScDotMessage m a) =
  ScDotMessage m (fmap scKeywordArgumentsRewriteKeyword a)

scExpressionRewriteKeyword :: ScExpression -> ScExpression
scExpressionRewriteKeyword e =
  case e of
    ScExprAssignment x y -> ScExprAssignment x (scExpressionRewriteKeyword y)
    ScExprBasic x -> ScExprBasic (scBasicExpressionRewriteKeyword x)

scTemporaryRewriteKeyword :: ScTemporary -> ScTemporary
scTemporaryRewriteKeyword (i,e) = (i,fmap scBasicExpressionRewriteKeyword e)

scTemporariesRewriteKeyword :: ScTemporaries -> ScTemporaries
scTemporariesRewriteKeyword = map scTemporaryRewriteKeyword

scReturnStatementRewriteKeyword :: ScReturnStatement -> ScReturnStatement
scReturnStatementRewriteKeyword (ScReturnStatement x) =
  (ScReturnStatement (scExpressionRewriteKeyword x))

scStatementsRewriteKeyword :: ScStatements -> ScStatements
scStatementsRewriteKeyword s =
  case s of
    ScStatementsReturn x -> ScStatementsReturn (scReturnStatementRewriteKeyword x)
    ScStatementsExpression x y -> ScStatementsExpression
                                  (scExpressionRewriteKeyword x)
                                  (fmap scStatementsRewriteKeyword y)

scBlockBodyRewriteKeyword :: ScBlockBody -> ScBlockBody
scBlockBodyRewriteKeyword (ScBlockBody a t s) =
  ScBlockBody a (fmap (map scTemporariesRewriteKeyword) t) (fmap scStatementsRewriteKeyword s)

scBasicExpressionRewriteKeyword :: ScBasicExpression -> ScBasicExpression
scBasicExpressionRewriteKeyword (ScBasicExpression p m) =
  ScBasicExpression (scPrimaryRewriteKeyword p) (fmap scMessagesRewriteKeyword m)

scPrimaryRewriteKeyword :: ScPrimary -> ScPrimary
scPrimaryRewriteKeyword p =
  case p of
    ScPrimaryIdentifier _ -> p
    ScPrimaryLiteral _ -> p
    ScPrimaryBlock x -> ScPrimaryBlock (scBlockBodyRewriteKeyword x)
    ScPrimaryExpression x -> ScPrimaryExpression (scExpressionRewriteKeyword x)
    ScPrimaryArrayExpression x -> ScPrimaryArrayExpression (map scBasicExpressionRewriteKeyword x)

scBinaryArgumentRewriteKeyword :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewriteKeyword (ScBinaryArgument p m) =
  ScBinaryArgument (scPrimaryRewriteKeyword p) (fmap (map scDotMessageRewriteKeyword) m)

scBinaryMessageRewriteKeyword :: ScBinaryMessage -> ScBinaryMessage
scBinaryMessageRewriteKeyword (ScBinaryMessage i a) =
  ScBinaryMessage i (scBinaryArgumentRewriteKeyword a)

scMessagesRewriteKeyword :: ScMessages -> ScMessages
scMessagesRewriteKeyword m =
  case m of
    ScMessagesDot m1 m2 -> ScMessagesDot
                           (map scDotMessageRewriteKeyword m1)
                           (fmap (map scBinaryMessageRewriteKeyword) m2)
    ScMessagesBinary m1 -> ScMessagesBinary (map scBinaryMessageRewriteKeyword m1)

-- | Viewer for keyword rewriter. Reads, rewrites and prints Sc expression.
scRewriteKeywordViewer :: String -> String
scRewriteKeywordViewer =
  Sc.scExpressionPrint .
  scExpressionRewriteKeyword .
  Sc.superColliderParser .
  Sc.alexScanTokens

{-

rw = scRewriteKeywordViewer
rw "p.q()" == "p.q([])"
rw "p.q(r)" == "p.q([r])"
rw "p.q(r:s)" == "p.q([\\r: -> s])"
rw "p.q(r:s.t)" == "p.q([\\r: -> (s.t)])"

-}
