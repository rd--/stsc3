module Language.Smalltalk.SuperCollider.Translate where

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Transform {- stsc3 -}

scPrimarySt :: ScPrimary -> St.Primary
scPrimarySt p =
  case p of
    ScPrimaryIdentifier x -> St.PrimaryIdentifier x
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

rw "1" == "1"
rw "p.q" == "p q"
rw "p.q + r" == "p q + r"
rw "p + q.r(a)" == "p + (q r: ({a}))"
rw "p + q.r.s(a)" == "p + (q r s: ({a}))"
rw "p + q.r.s(a).t" == "p + (q r s: ({a})) t"
rw "p + q.r.s(a).t.u(b)" == "p + ((q r s: ({a})) t u: ({b}))"
rw "p.q(a)" == "p q: ({a})"
rw "p.q(a).r" == "(p q: ({a})) r"
rw "p.q(a).r(b)" == "(p q: ({a})) r: ({b})"
rw "p + q" == "p + q"
rw "p.q + r" == "p q + r"
rw "p.q(a) + r" == "(p q: ({a})) + r"
rw "p.q(a) + r.s(b)" == "(p q: ({a})) + (r s: ({b}))"
rw "{p.q(a).r}" == "[ (p q: ({a})) r .\n ]"
rw "{var x = p.q(a).r; x}" == "[ |x|\n x := (p q: ({a})) r .\n x .\n ]"

rw "{var x = a; x}"
rw "{var x = a,y; x + y}"
rw "{var x; var y = b; x + y}"
rw "{var x = {var y = a; a * x}.value; x}"

rw "p.q()"
rw "p.q(a)"
rw "p.q(a,b)"
rw "p.q(x:a,b)"
rw "p.q(a,x:b)"
rw "p.q(a,x: b.c(y: d,e))"

-}
