-- | Translate from the SuperCollider (Sc) Ast to the Smalltalk (St) Ast.
module Language.Smalltalk.SuperCollider.Translate where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}

{- | This is for translating, it allows either
     a Unary sequence with an optional ending Keyword,
     or a single trailing Keyword.
-}
scDotMessagesForSmalltalk :: [ScDotMessage] -> ([ScDotMessage], Maybe ScDotMessage)
scDotMessagesForSmalltalk m =
  if scDotMessagesHaveKeyword m
  then case break scDotMessageIsKeyword m of
         (lhs,[]) -> (lhs,Nothing)
         (lhs,[k]) -> (lhs,Just k)
         _ -> error "scDotMessagesForSmalltalk"
  else (m,Nothing)

scPiSt :: St.Primary
scPiSt =
  St.PrimaryExpression
  (St.ExprBasic
    (St.BasicExpression
      (St.PrimaryIdentifier "Float")
      (Just (St.MessagesUnary [St.UnaryMessage "pi"] Nothing Nothing))
      Nothing))

scPrimarySt :: ScPrimary -> St.Primary
scPrimarySt p =
  case p of
    ScPrimaryIdentifier "pi" -> scPiSt
    ScPrimaryIdentifier x ->
        case x of
          "this" -> St.PrimaryIdentifier "self"
          "inf" -> St.PrimaryIdentifier "Infinity"
          "pi" -> scPiSt
          _ -> St.PrimaryIdentifier x
    ScPrimaryLiteral x -> St.PrimaryLiteral x
    ScPrimaryBlock x -> St.PrimaryBlock (scBlockBodySt x)
    ScPrimaryExpression x -> St.PrimaryExpression (scExpressionSt x)
    ScPrimaryArrayExpression x -> St.PrimaryArrayExpression (map scBasicExpressionSt x)

scBlockBodySt :: ScBlockBody -> St.BlockBody
scBlockBodySt (ScBlockBody arg tmp stm) =
  St.BlockBody arg (fmap scTemporariesSt tmp) (fmap scStatementsSt stm)

scBinaryArgumentSt :: ScBinaryArgument -> St.BinaryArgument
scBinaryArgumentSt (ScBinaryArgument p m) =
  case m of
    Nothing -> St.BinaryArgument (scPrimarySt p) Nothing
    Just d ->
      case scDotMessagesForSmalltalk d of
        (u,Nothing) -> St.BinaryArgument (scPrimarySt p) (Just (map scDotMessageUnarySt u))
        _ -> error ("scBinaryArgumentSt: " ++ show (ScBinaryArgument p m))

scBinaryMessageSt :: ScBinaryMessage -> St.BinaryMessage
scBinaryMessageSt (ScBinaryMessage i a) = St.BinaryMessage i (scBinaryArgumentSt a)

scKeywordArgumentSt :: ScKeywordArgument -> St.KeywordArgument
scKeywordArgumentSt (ScKeywordArgument k e) =
  case k of
    Just _ -> error "scKeywordArgumentSt"
    Nothing -> St.KeywordArgument
               (St.PrimaryExpression (St.ExprBasic (scBasicExpressionSt e)))
               Nothing
               Nothing

scDotMessageKeywordSt :: ScDotMessage -> St.KeywordMessage
scDotMessageKeywordSt (ScDotMessage i a) =
  case a of
    Nothing -> error "scDotMessageKeywordSt: Unary"
    Just [p] -> St.KeywordMessage [(i ++ ":",scKeywordArgumentSt p)]
    _ -> error "scDotMessageKeywordSt: non Array"

scDotMessageUnarySt :: ScDotMessage -> St.UnaryMessage
scDotMessageUnarySt (ScDotMessage i a) =
  case a of
    Nothing -> St.UnaryMessage i
    _ -> error ("scDotMessageUnarySt: argument: " ++ show a)

scMessagesSt :: ScMessages -> St.Messages
scMessagesSt m =
  case m of
    ScMessagesDot m1 m2 ->
      case scDotMessagesForSmalltalk m1 of
        ([],Just k) -> St.MessagesKeyword (scDotMessageKeywordSt k)
        (u,k) -> St.MessagesUnary
                 (map scDotMessageUnarySt u)
                 (fmap (map scBinaryMessageSt) m2)
                 (fmap scDotMessageKeywordSt k)
    ScMessagesBinary m1 -> St.MessagesBinary (map scBinaryMessageSt m1) Nothing

scTemporarySt :: ScTemporary -> St.Identifier
scTemporarySt t =
  case t of
    (i,Nothing) -> i
    _ -> error "scTemporarySt"

-- | Temporaries must be in correct form.
scTemporariesSt :: [ScTemporaries] -> St.Temporaries
scTemporariesSt t =
  case t of
    [t1] -> St.Temporaries (map scTemporarySt t1)
    _ -> error "scTemporariesSt"

scReturnStatementSt :: ScReturnStatement -> St.ReturnStatement
scReturnStatementSt (ScReturnStatement e) = St.ReturnStatement (scExpressionSt e)

scStatementsSt :: ScStatements -> St.Statements
scStatementsSt stm =
  case stm of
    ScStatementsReturn x -> St.StatementsReturn (scReturnStatementSt x)
    ScStatementsExpression x y -> St.StatementsExpression (scExpressionSt x) (fmap scStatementsSt y)

scReturnStatement :: ScReturnStatement -> St.ReturnStatement
scReturnStatement (ScReturnStatement x) = St.ReturnStatement (scExpressionSt x)

scBasicExpressionSt :: ScBasicExpression -> St.BasicExpression
scBasicExpressionSt (ScBasicExpression x y) =
  St.BasicExpression (scPrimarySt x) (fmap scMessagesSt y) Nothing

scExpressionSt :: ScExpression -> St.Expression
scExpressionSt e =
  case e of
    ScExprAssignment i e1 -> St.ExprAssignment (St.Assignment i (scExpressionSt e1))
    ScExprBasic e1 -> St.ExprBasic (scBasicExpressionSt e1)

{-

import Language.Smalltalk.SuperCollider.Ast.Print
import Language.Smalltalk.SuperCollider.Lexer
import Language.Smalltalk.SuperCollider.Parser

import qualified Language.Smalltalk.Ansi.Print as St

p = superColliderParser . alexScanTokens
x = scExpressionPrint . p
rw = St.expression_pp . scExpressionSt . scExpressionRewrite . p

rw "p + q.r.s(a).t.u(b)" == "p + ((q r s: ({a})) t u: ({b}))"

-}
