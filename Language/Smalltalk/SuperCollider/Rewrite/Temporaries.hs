{- | Temporaries re-writing

SuperCollider allows assignment at variable declarations.
The Identifiers from the sequence are collected as Temporaries.
Initialisation expressions are prepended as Assignments to the list of Statements.
-}
module Language.Smalltalk.SuperCollider.Rewrite.Temporaries where

import Data.Maybe {- maybe -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

-- | Get initialiser lifted to an Assignment expression.
scTemporaryInitialiser :: ScTemporary -> Maybe ScExpression
scTemporaryInitialiser (k,v) =
  let f e = ScExprAssignment k (ScExprBasic (scBasicExpressionRewriteTemporaries e))
  in fmap f v

scTemporariesPartition :: [ScTemporaries] -> ([St.Identifier], [ScExpression])
scTemporariesPartition tmp =
  let tmpNames = map fst (concat tmp)
      tmpExpr = mapMaybe scTemporaryInitialiser (concat tmp)
  in (tmpNames,tmpExpr)

scBlockBodyRewriteTemporaries :: ScBlockBody -> ScBlockBody
scBlockBodyRewriteTemporaries (ScBlockBody arg tmpMaybe stm) =
  let stmRw = fmap scStatementsRewriteTemporaries stm
  in case tmpMaybe of
       Nothing -> ScBlockBody arg Nothing stmRw
       Just tmp ->
         let (tmpNames,tmpExpr) = scTemporariesPartition tmp
             idToTmp k = (k,Nothing)
             maybeTmp = if null tmpNames
                        then error "scBlockBodyRewriteTemporaries"
                        else Just [map idToTmp tmpNames]
             tmpExprRw = map scExpressionRewriteTemporaries tmpExpr
         in case tmpExpr of
           [] -> ScBlockBody arg maybeTmp stmRw
           _ -> ScBlockBody
                arg
                maybeTmp
                (Just (scExpressionSequenceToStatements stmRw tmpExprRw))

scStatementsRewriteTemporaries :: ScStatements -> ScStatements
scStatementsRewriteTemporaries s =
    case s of
      ScStatementsReturn (ScReturnStatement e) ->
        ScStatementsReturn (ScReturnStatement (scExpressionRewriteTemporaries e))
      ScStatementsExpression e s' ->
        ScStatementsExpression
        (scExpressionRewriteTemporaries e)
        (fmap scStatementsRewriteTemporaries s')

scBinaryArgumentRewriteTemporaries :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewriteTemporaries (ScBinaryArgument p m) =
  ScBinaryArgument (scPrimaryRewriteTemporaries p) (fmap (map scDotMessageRewriteTemporaries) m)

scKeywordArgumentRewriteTemporaries :: ScKeywordArgument -> ScKeywordArgument
scKeywordArgumentRewriteTemporaries (ScKeywordArgument k v) =
  ScKeywordArgument k (scBasicExpressionRewriteTemporaries v)

scDotMessageRewriteTemporaries :: ScDotMessage -> ScDotMessage
scDotMessageRewriteTemporaries (ScDotMessage i a) =
  ScDotMessage i (fmap (map scKeywordArgumentRewriteTemporaries) a)

scBinaryMessageRewriteTemporaries :: ScBinaryMessage -> ScBinaryMessage
scBinaryMessageRewriteTemporaries (ScBinaryMessage i a) =
  ScBinaryMessage i (scBinaryArgumentRewriteTemporaries a)

scMessagesRewriteTemporaries :: ScMessages -> ScMessages
scMessagesRewriteTemporaries m =
  case m of
    ScMessagesDot m1 m2 -> ScMessagesDot
                           (map scDotMessageRewriteTemporaries m1)
                           (fmap (map scBinaryMessageRewriteTemporaries) m2)
    ScMessagesBinary m1 -> ScMessagesBinary (map scBinaryMessageRewriteTemporaries m1)

scBasicExpressionRewriteTemporaries :: ScBasicExpression -> ScBasicExpression
scBasicExpressionRewriteTemporaries (ScBasicExpression p m) =
      ScBasicExpression (scPrimaryRewriteTemporaries p) (fmap scMessagesRewriteTemporaries m)

scExpressionRewriteTemporaries :: ScExpression -> ScExpression
scExpressionRewriteTemporaries e =
  case e of
    ScExprAssignment x y -> ScExprAssignment x (scExpressionRewriteTemporaries y)
    ScExprBasic x -> ScExprBasic (scBasicExpressionRewriteTemporaries x)

scPrimaryRewriteTemporaries :: ScPrimary -> ScPrimary
scPrimaryRewriteTemporaries p =
  case p of
    ScPrimaryIdentifier _ -> p
    ScPrimaryLiteral _ -> p
    ScPrimaryBlock x -> ScPrimaryBlock (scBlockBodyRewriteTemporaries x)
    ScPrimaryExpression x -> ScPrimaryExpression (scExpressionRewriteTemporaries x)
    ScPrimaryArrayExpression x -> ScPrimaryArrayExpression (map scBasicExpressionRewriteTemporaries x)

-- | Viewer for temporaries rewriter.  Reads, rewrites and prints Sc expression.
scRewriteTemporariesViewer :: String -> String
scRewriteTemporariesViewer =
  Sc.scExpressionPrint .
  scExpressionRewriteTemporaries .
  Sc.superColliderParser .
  Sc.alexScanTokens

{-
rw = scRewriteTemporariesViewer
-}
