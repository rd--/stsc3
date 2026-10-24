{- | Temporaries re-writing

Spl allows assignment at variable declarations.
The Identifiers from the sequence are collected as Temporaries.
Initialisation expressions are prepended as Assignments to the list of Statements.
-}
module Language.Smalltalk.Spl.Rewrite.Temporaries where

import Data.Maybe {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Spl.Ast {- stsc3 -}
import qualified Language.Smalltalk.Spl.Ast.Print as Spl {- stsc3 -}
import qualified Language.Smalltalk.Spl.Lexer as Spl {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Spl {- stsc3 -}

-- | Get initialiser lifted to an Assignment expression.
splTemporaryInitialiser :: SplTemporary -> Maybe SplExpression
splTemporaryInitialiser (k, v) =
  let f e = SplExprAssignment k (SplExprBasic (splBasicExpressionRewriteTemporaries e))
  in fmap f v

splTemporariesPartition :: [SplTemporaries] -> ([St.Identifier], [SplExpression])
splTemporariesPartition tmp =
  let tmpNames = map fst (concat tmp)
      tmpExpr = mapMaybe splTemporaryInitialiser (concat tmp)
  in (tmpNames, tmpExpr)

splTmpStmRwTmp :: Maybe [SplTemporaries] -> Maybe SplStatements -> (Maybe [SplTemporaries], Maybe SplStatements)
splTmpStmRwTmp tmpMaybe stmMaybe =
  let stmRw = fmap splStatementsRewriteTemporaries stmMaybe
  in case tmpMaybe of
      Nothing -> (Nothing, stmRw)
      Just tmp ->
        let (tmpNames, tmpExpr) = splTemporariesPartition tmp
            idToTmp k = (k, Nothing)
            maybeTmp =
              if null tmpNames
                then error "splTmpStmRwTmp"
                else Just [map idToTmp tmpNames]
            tmpExprRw = map splExpressionRewriteTemporaries tmpExpr
        in case tmpExpr of
            [] -> (maybeTmp, stmRw)
            _ -> (maybeTmp, Just (splExpressionSequenceToStatements stmRw tmpExprRw))

splBlockBodyRewriteTemporaries :: SplBlockBody -> SplBlockBody
splBlockBodyRewriteTemporaries (SplBlockBody arg tmpMaybe stmMaybe) =
  let (tmp, stm) = splTmpStmRwTmp tmpMaybe stmMaybe
  in SplBlockBody arg tmp stm

splStatementsRewriteTemporaries :: SplStatements -> SplStatements
splStatementsRewriteTemporaries s =
  case s of
    SplStatementsReturn (SplReturnStatement e) ->
      SplStatementsReturn (SplReturnStatement (splExpressionRewriteTemporaries e))
    SplStatementsExpression e s' ->
      SplStatementsExpression
        (splExpressionRewriteTemporaries e)
        (fmap splStatementsRewriteTemporaries s')

splBinaryArgumentRewriteTemporaries :: SplBinaryArgument -> SplBinaryArgument
splBinaryArgumentRewriteTemporaries (SplBinaryArgument p m) =
  SplBinaryArgument (splPrimaryRewriteTemporaries p) (fmap (map splDotMessageRewriteTemporaries) m)

splDotMessageRewriteTemporaries :: SplDotMessage -> SplDotMessage
splDotMessageRewriteTemporaries (SplDotMessage i a) =
  SplDotMessage i (map splBasicExpressionRewriteTemporaries a)

splBinaryMessageRewriteTemporaries :: SplBinaryMessage -> SplBinaryMessage
splBinaryMessageRewriteTemporaries (SplBinaryMessage i a) =
  SplBinaryMessage i (splBinaryArgumentRewriteTemporaries a)

splMessagesRewriteTemporaries :: SplMessages -> SplMessages
splMessagesRewriteTemporaries m =
  case m of
    SplMessagesDot m1 m2 ->
      SplMessagesDot
        (map splDotMessageRewriteTemporaries m1)
        (fmap (map splBinaryMessageRewriteTemporaries) m2)
    SplMessagesBinary m1 -> SplMessagesBinary (map splBinaryMessageRewriteTemporaries m1)

splBasicExpressionRewriteTemporaries :: SplBasicExpression -> SplBasicExpression
splBasicExpressionRewriteTemporaries (SplBasicExpression p m) =
  SplBasicExpression (splPrimaryRewriteTemporaries p) (fmap splMessagesRewriteTemporaries m)

splExpressionRewriteTemporaries :: SplExpression -> SplExpression
splExpressionRewriteTemporaries e =
  case e of
    SplExprAssignment x y -> SplExprAssignment x (splExpressionRewriteTemporaries y)
    SplExprBasic x -> SplExprBasic (splBasicExpressionRewriteTemporaries x)

splInitializerDefinitionRewriteTemporaries :: SplInitializerDefinition -> SplInitializerDefinition
splInitializerDefinitionRewriteTemporaries (SplInitializerDefinition cmt tmpMaybe stmMaybe) =
  let (tmp, stm) = splTmpStmRwTmp tmpMaybe stmMaybe
  in SplInitializerDefinition cmt tmp stm

splPrimaryRewriteTemporaries :: SplPrimary -> SplPrimary
splPrimaryRewriteTemporaries p =
  case p of
    SplPrimaryIdentifier _ -> p
    SplPrimaryLiteral _ -> p
    SplPrimaryBlock x -> SplPrimaryBlock (splBlockBodyRewriteTemporaries x)
    SplPrimaryExpression x -> SplPrimaryExpression (splExpressionRewriteTemporaries x)
    SplPrimaryArrayExpression x -> SplPrimaryArrayExpression (map splBasicExpressionRewriteTemporaries x)
    SplPrimaryDictionaryExpression x -> SplPrimaryDictionaryExpression (map (\(k, v) -> (k, splBasicExpressionRewriteTemporaries v)) x)
    SplPrimaryImplicitMessageSend x a -> SplPrimaryImplicitMessageSend x (map splBasicExpressionRewriteTemporaries a)

-- | Viewer for temporaries rewriter.  Reads, rewrites and prints Sc expression.
splRewriteTemporariesViewer :: String -> String
splRewriteTemporariesViewer =
  Spl.splInitializerDefinitionPrint
    . splInitializerDefinitionRewriteTemporaries
    . Spl.parseInitializerDefinition
    . Spl.alexScanTokens

{-
rw = splRewriteTemporariesViewer
-}
