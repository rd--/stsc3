{- | Temporaries re-writing

Stc allows assignment at variable declarations.
The Identifiers from the sequence are collected as Temporaries.
Initialisation expressions are prepended as Assignments to the list of Statements.
-}
module Language.Smalltalk.Stc.Rewrite.Temporaries where

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Stc.Ast {- stsc3 -}
import qualified Language.Smalltalk.Stc.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Parser as Sc {- stsc3 -}

-- | Get initialiser lifted to an Assignment expression.
stcTemporaryInitialiser :: StcTemporary -> Maybe StcExpression
stcTemporaryInitialiser (k, v) =
  let f e = StcExprAssignment k (StcExprBasic (stcBasicExpressionRewriteTemporaries e))
  in fmap f v

stcTemporariesPartition :: [StcTemporaries] -> ([St.Identifier], [StcExpression])
stcTemporariesPartition tmp =
  let tmpNames = map fst (concat tmp)
      tmpExpr = mapMaybe stcTemporaryInitialiser (concat tmp)
  in (tmpNames, tmpExpr)

stcTmpStmRwTmp :: Maybe [StcTemporaries] -> Maybe StcStatements -> (Maybe [StcTemporaries], Maybe StcStatements)
stcTmpStmRwTmp tmpMaybe stmMaybe =
  let stmRw = fmap stcStatementsRewriteTemporaries stmMaybe
  in case tmpMaybe of
      Nothing -> (Nothing, stmRw)
      Just tmp ->
        let (tmpNames, tmpExpr) = stcTemporariesPartition tmp
            idToTmp k = (k, Nothing)
            maybeTmp =
              if null tmpNames
                then error "stcTmpStmRwTmp"
                else Just [map idToTmp tmpNames]
            tmpExprRw = map stcExpressionRewriteTemporaries tmpExpr
        in case tmpExpr of
            [] -> (maybeTmp, stmRw)
            _ -> (maybeTmp, Just (stcExpressionSequenceToStatements stmRw tmpExprRw))

stcBlockBodyRewriteTemporaries :: StcBlockBody -> StcBlockBody
stcBlockBodyRewriteTemporaries (StcBlockBody arg tmpMaybe stmMaybe) =
  let (tmp, stm) = stcTmpStmRwTmp tmpMaybe stmMaybe
  in StcBlockBody arg tmp stm

stcStatementsRewriteTemporaries :: StcStatements -> StcStatements
stcStatementsRewriteTemporaries s =
  case s of
    StcStatementsReturn (StcReturnStatement e) ->
      StcStatementsReturn (StcReturnStatement (stcExpressionRewriteTemporaries e))
    StcStatementsExpression e s' ->
      StcStatementsExpression
        (stcExpressionRewriteTemporaries e)
        (fmap stcStatementsRewriteTemporaries s')

stcBinaryArgumentRewriteTemporaries :: StcBinaryArgument -> StcBinaryArgument
stcBinaryArgumentRewriteTemporaries (StcBinaryArgument p m) =
  StcBinaryArgument (stcPrimaryRewriteTemporaries p) (fmap (map stcDotMessageRewriteTemporaries) m)

stcDotMessageRewriteTemporaries :: StcDotMessage -> StcDotMessage
stcDotMessageRewriteTemporaries (StcDotMessage i a) =
  StcDotMessage i (map stcBasicExpressionRewriteTemporaries a)

stcBinaryMessageRewriteTemporaries :: StcBinaryMessage -> StcBinaryMessage
stcBinaryMessageRewriteTemporaries (StcBinaryMessage i a) =
  StcBinaryMessage i (stcBinaryArgumentRewriteTemporaries a)

stcMessagesRewriteTemporaries :: StcMessages -> StcMessages
stcMessagesRewriteTemporaries m =
  case m of
    StcMessagesDot m1 m2 ->
      StcMessagesDot
        (map stcDotMessageRewriteTemporaries m1)
        (fmap (map stcBinaryMessageRewriteTemporaries) m2)
    StcMessagesBinary m1 -> StcMessagesBinary (map stcBinaryMessageRewriteTemporaries m1)

stcBasicExpressionRewriteTemporaries :: StcBasicExpression -> StcBasicExpression
stcBasicExpressionRewriteTemporaries (StcBasicExpression p m) =
  StcBasicExpression (stcPrimaryRewriteTemporaries p) (fmap stcMessagesRewriteTemporaries m)

stcExpressionRewriteTemporaries :: StcExpression -> StcExpression
stcExpressionRewriteTemporaries e =
  case e of
    StcExprAssignment x y -> StcExprAssignment x (stcExpressionRewriteTemporaries y)
    StcExprBasic x -> StcExprBasic (stcBasicExpressionRewriteTemporaries x)

stcInitializerDefinitionRewriteTemporaries :: StcInitializerDefinition -> StcInitializerDefinition
stcInitializerDefinitionRewriteTemporaries (StcInitializerDefinition cmt tmpMaybe stmMaybe) =
  let (tmp, stm) = stcTmpStmRwTmp tmpMaybe stmMaybe
  in StcInitializerDefinition cmt tmp stm

stcPrimaryRewriteTemporaries :: StcPrimary -> StcPrimary
stcPrimaryRewriteTemporaries p =
  case p of
    StcPrimaryIdentifier _ -> p
    StcPrimaryLiteral _ -> p
    StcPrimaryBlock x -> StcPrimaryBlock (stcBlockBodyRewriteTemporaries x)
    StcPrimaryExpression x -> StcPrimaryExpression (stcExpressionRewriteTemporaries x)
    StcPrimaryArrayExpression x -> StcPrimaryArrayExpression (map stcBasicExpressionRewriteTemporaries x)
    StcPrimaryDictionaryExpression x -> StcPrimaryDictionaryExpression (map (\(k, v) -> (k, stcBasicExpressionRewriteTemporaries v)) x)
    StcPrimaryImplicitMessageSend x a -> StcPrimaryImplicitMessageSend x (map stcBasicExpressionRewriteTemporaries a)

-- | Viewer for temporaries rewriter.  Reads, rewrites and prints Sc expression.
stcRewriteTemporariesViewer :: String -> String
stcRewriteTemporariesViewer =
  Sc.stcInitializerDefinitionPrint
    . stcInitializerDefinitionRewriteTemporaries
    . Sc.stcParserInitializerDefinition
    . Sc.alexScanTokens

{-
rw = stcRewriteTemporariesViewer
-}
