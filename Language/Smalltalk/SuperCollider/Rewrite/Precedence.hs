{- | Precedence re-writing
     The recursion rules here are intricate and likely incorrect.
-}
module Language.Smalltalk.SuperCollider.Rewrite.Precedence where

import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

{- | This is for parenthesising.
     It places the initial keyword at the end of the lhs, not the start of the rhs.
-}
scDotMessagesSplitAtKeyword :: [ScDotMessage] -> ([ScDotMessage], [ScDotMessage])
scDotMessagesSplitAtKeyword m =
  case break scDotMessageIsKeyword m of
    (lhs,k:rhs) -> (lhs ++ [k],rhs)
    _ -> error "scDotMessagesSplitAtKeyword?"

scBinaryArgumentRewritePrecedence :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewritePrecedence (ScBinaryArgument p m) =
  case m of
    Nothing -> ScBinaryArgument (scPrimaryRewritePrecedence p) Nothing
    Just x -> if not (scDotMessagesHaveKeyword x)
              then ScBinaryArgument (scPrimaryRewritePrecedence p) m
              else let (lhs,rhs) = scDotMessagesSplitAtKeyword x
                   in (ScBinaryArgument
                       (ScPrimaryExpression
                         (ScExprBasic
                          (ScBasicExpression
                            (scPrimaryRewritePrecedence p)
                            (Just (ScMessagesDot (map scDotMessageRewritePrecedence lhs) Nothing)))))
                       (Just (map scDotMessageRewritePrecedence rhs)))

scBinaryMessageRewritePrecedence :: ScBinaryMessage -> ScBinaryMessage
scBinaryMessageRewritePrecedence (ScBinaryMessage i a) =
  ScBinaryMessage i (scBinaryArgumentRewritePrecedence a)

scBasicExpressionRewritePrecedence :: ScBasicExpression -> ScBasicExpression
scBasicExpressionRewritePrecedence (ScBasicExpression p m) =
  case m of
    Nothing ->
      ScBasicExpression (scPrimaryRewritePrecedence p) Nothing
    Just (ScMessagesBinary b) ->
      ScBasicExpression
      (scPrimaryRewritePrecedence p)
      (Just (ScMessagesBinary (map scBinaryMessageRewritePrecedence b)))
    Just (ScMessagesDot d Nothing) ->
      if not (scDotMessagesHaveKeyword d) || length d == 1
      then ScBasicExpression
           (scPrimaryRewritePrecedence p)
           (Just (ScMessagesDot (map scDotMessageRewritePrecedence d) Nothing))
      else let (lhs,rhs) = scDotMessagesSplitAtKeyword d
           in scBasicExpressionRewritePrecedence
              (ScBasicExpression
                (ScPrimaryExpression
                  (ScExprBasic
                    (ScBasicExpression p (Just (ScMessagesDot lhs Nothing)))))
                (Just (ScMessagesDot rhs Nothing)))
    Just (ScMessagesDot d (Just b)) ->
      if not (scDotMessagesHaveKeyword d) || length d == 1
      then ScBasicExpression
           (scPrimaryRewritePrecedence p)
           (Just
             (ScMessagesDot
               (map scDotMessageRewritePrecedence d)
               (Just (map scBinaryMessageRewritePrecedence b))))
      else let (lhs,rhs) = scDotMessagesSplitAtKeyword d
           in scBasicExpressionRewritePrecedence
              (ScBasicExpression
                (ScPrimaryExpression
                  (ScExprBasic
                    (ScBasicExpression p (Just (ScMessagesDot lhs Nothing)))))
                (Just (ScMessagesDot rhs (Just b))))

scExpressionRewritePrecedence :: ScExpression -> ScExpression
scExpressionRewritePrecedence e =
  case e of
    ScExprAssignment x y -> ScExprAssignment x (scExpressionRewritePrecedence y)
    ScExprBasic x -> ScExprBasic (scBasicExpressionRewritePrecedence x)

scKeywordArgumentRewritePrecedence :: ScKeywordArgument -> ScKeywordArgument
scKeywordArgumentRewritePrecedence (ScKeywordArgument k v) =
  ScKeywordArgument k (scBasicExpressionRewritePrecedence v)

scDotMessageRewritePrecedence :: ScDotMessage -> ScDotMessage
scDotMessageRewritePrecedence (ScDotMessage i a) =
  ScDotMessage i (fmap (map scKeywordArgumentRewritePrecedence) a)

scMessagesRewritePrecedence :: ScMessages -> ScMessages
scMessagesRewritePrecedence m =
  case m of
    ScMessagesDot m1 m2 -> ScMessagesDot
                           (map scDotMessageRewritePrecedence m1)
                           (fmap (map scBinaryMessageRewritePrecedence) m2)
    ScMessagesBinary m1 -> ScMessagesBinary (map scBinaryMessageRewritePrecedence m1)


scReturnStatementRewritePrecedence :: ScReturnStatement -> ScReturnStatement
scReturnStatementRewritePrecedence (ScReturnStatement x) =
  (ScReturnStatement (scExpressionRewritePrecedence x))

scStatementsRewritePrecedence :: ScStatements -> ScStatements
scStatementsRewritePrecedence s =
  case s of
    ScStatementsReturn x -> ScStatementsReturn (scReturnStatementRewritePrecedence x)
    ScStatementsExpression x y -> ScStatementsExpression
                                  (scExpressionRewritePrecedence x)
                                  (fmap scStatementsRewritePrecedence y)

scTemporaryRewritePrecedence :: ScTemporary -> ScTemporary
scTemporaryRewritePrecedence (i,e) = (i,fmap scBasicExpressionRewritePrecedence e)

scTemporariesRewritePrecedence :: ScTemporaries -> ScTemporaries
scTemporariesRewritePrecedence = map scTemporaryRewritePrecedence

scBlockBodyRewritePrecedence :: ScBlockBody -> ScBlockBody
scBlockBodyRewritePrecedence (ScBlockBody a t s) =
  ScBlockBody a (fmap (map scTemporariesRewritePrecedence) t) (fmap scStatementsRewritePrecedence s)

scPrimaryRewritePrecedence :: ScPrimary -> ScPrimary
scPrimaryRewritePrecedence p =
  case p of
    ScPrimaryIdentifier _ -> p
    ScPrimaryLiteral _ -> p
    ScPrimaryBlock x -> ScPrimaryBlock (scBlockBodyRewritePrecedence x)
    ScPrimaryExpression x -> ScPrimaryExpression (scExpressionRewritePrecedence x)
    ScPrimaryArrayExpression x -> ScPrimaryArrayExpression (map scBasicExpressionRewritePrecedence x)

-- | Test procedure, reads rewrites and prints Sc expression.
scPrecedenceViewer :: String -> String
scPrecedenceViewer =
  Sc.scExpressionPrint .
  scExpressionRewritePrecedence .
  Sc.superColliderParser .
  Sc.alexScanTokens

{-

rw = scPrecedenceViewer
rw "p.q()" == "p.q()" -- only one message, can be keyword
rw "p.q().r()" == "(p.q()).r()" -- nested trailing
rw "p.q + r" == "p.q + r" -- unary no parens
rw "p.q() + r" == "(p.q()) + r" -- parens ; singular requires if initial of binary, c.f. p.q(a)
rw "p + q.r" == "p + q.r"
rw "p + q.r()" == "p + (q.r())"
rw "p + q.r().s" == "p + (q.r()).s"
rw "p.q(r + s.t())" == "p.q(r + (s.t()))" -- Binary within Dot/Keyword
rw "p.q(a + b + c + d * e).r * s" == "(p.q(a + b + c + d * e)).r * s"
rw "p.q().r" == "(p.q()).r"
rw "p.q().r + s" == "(p.q()).r + s"

rd = Sc.superColliderParser . Sc.alexScanTokens
rd "p.q().r"
rd "p.q().r + s"

-}
