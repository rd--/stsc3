{- | Precedence re-writing.
     The rewriting here is quite intricate.
     It goes to a little trouble to not introduce unnecessary parentheses.
-}
module Language.Smalltalk.SuperCollider.Rewrite.Precedence where

import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

{- | This is for parenthesising.
     It places the initial nary selector at the end of the lhs, not the start of the rhs.
     If the rhs is null then the rewriting should halt (ie. null rhs is the recurrence check).
-}
scDotMessagesSplitAtNary :: [ScDotMessage] -> ([ScDotMessage], [ScDotMessage])
scDotMessagesSplitAtNary m =
  case break scDotMessageIsNary m of
    (lhs,k:rhs) -> (lhs ++ [k],rhs)
    _ -> error "scDotMessagesSplitAtNary?"

scPrimaryRewritePrecedenceMaybe :: Bool -> ScPrimary -> ScPrimary
scPrimaryRewritePrecedenceMaybe rw = if rw then scPrimaryRewritePrecedence else id

scDotMessageRewritePrecedenceMaybe :: Bool -> ScDotMessage -> ScDotMessage
scDotMessageRewritePrecedenceMaybe rw = if rw then scDotMessageRewritePrecedence else id

scBinaryMessageRewritePrecedenceMaybe :: Bool -> ScBinaryMessage -> ScBinaryMessage
scBinaryMessageRewritePrecedenceMaybe rw = if rw then scBinaryMessageRewritePrecedence else id

scBinaryArgumentRewritePrecedence :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewritePrecedence (ScBinaryArgument p m) =
  case m of
    Nothing ->
      ScBinaryArgument (scPrimaryRewritePrecedence p) Nothing
    Just x ->
      if not (scDotMessagesHaveNary x)
      then ScBinaryArgument
           (scPrimaryRewritePrecedence p)
           (Just (map scDotMessageRewritePrecedence x))
      else let (lhs,rhs) = scDotMessagesSplitAtNary x
               rw = null rhs
               dmRw = scDotMessageRewritePrecedenceMaybe rw
           in (if rw then id else scBinaryArgumentRewritePrecedence)
              (ScBinaryArgument
                (ScPrimaryExpression
                  (ScExprBasic
                    (ScBasicExpression
                      (scPrimaryRewritePrecedenceMaybe rw p)
                      (Just (ScMessagesDot (map dmRw lhs) Nothing)))))
                (Just (map dmRw rhs)))

scBinaryMessageRewritePrecedence :: ScBinaryMessage -> ScBinaryMessage
scBinaryMessageRewritePrecedence (ScBinaryMessage i a) =
  ScBinaryMessage i (scBinaryArgumentRewritePrecedence a)

-- | Outer (initial) n-ary messages don't require parentheses.
scBasicExpressionRewritePrecedence :: Bool -> ScBasicExpression -> ScBasicExpression
scBasicExpressionRewritePrecedence isInit (ScBasicExpression p m) =
  case m of
    Nothing ->
      ScBasicExpression (scPrimaryRewritePrecedence p) Nothing
    Just (ScMessagesBinary b) ->
      ScBasicExpression
      (scPrimaryRewritePrecedence p)
      (Just (ScMessagesBinary (map scBinaryMessageRewritePrecedence b)))
    Just (ScMessagesDot d Nothing) ->
      if (isInit && length d == 1) || not (scDotMessagesHaveNary d)
      then ScBasicExpression
           (scPrimaryRewritePrecedence p)
           (Just (ScMessagesDot (map scDotMessageRewritePrecedence d) Nothing))
      else let (lhs,rhs) = scDotMessagesSplitAtNary d
               rw = null rhs
               dmRw = scDotMessageRewritePrecedenceMaybe rw
           in (if rw then id else scBasicExpressionRewritePrecedence isInit)
              (ScBasicExpression
               (ScPrimaryExpression
                (ScExprBasic
                 (ScBasicExpression
                   (scPrimaryRewritePrecedenceMaybe rw p)
                   (Just (ScMessagesDot (map dmRw lhs) Nothing)))))
               (Just (ScMessagesDot (map dmRw rhs) Nothing)))
    Just (ScMessagesDot d (Just b)) ->
      if not (scDotMessagesHaveNary d)
      then ScBasicExpression
           (scPrimaryRewritePrecedence p)
           (Just
             (ScMessagesDot
               (map scDotMessageRewritePrecedence d)
               (Just (map scBinaryMessageRewritePrecedence b))))
      else let (lhs,rhs) = scDotMessagesSplitAtNary d
               rw = null rhs
               dmRw = scDotMessageRewritePrecedenceMaybe rw
           in (if rw then id else scBasicExpressionRewritePrecedence False)
              (ScBasicExpression
                (ScPrimaryExpression
                  (ScExprBasic
                    (ScBasicExpression
                     (scPrimaryRewritePrecedenceMaybe rw p)
                      (Just (ScMessagesDot (map dmRw lhs) Nothing)))))
                (Just (ScMessagesDot
                        (map dmRw rhs)
                        (Just (map (scBinaryMessageRewritePrecedenceMaybe rw) b)))))

scExpressionRewritePrecedence :: ScExpression -> ScExpression
scExpressionRewritePrecedence e =
  case e of
    ScExprAssignment x y -> ScExprAssignment x (scExpressionRewritePrecedence y)
    ScExprBasic x -> ScExprBasic (scBasicExpressionRewritePrecedence True x)

scInitializerDefinitionRewritePrecedence :: ScInitializerDefinition -> ScInitializerDefinition
scInitializerDefinitionRewritePrecedence (ScInitializerDefinition cmt tmp stm) =
  ScInitializerDefinition
  cmt
  (fmap (map scTemporariesRewritePrecedence) tmp)
  (fmap scStatementsRewritePrecedence stm)

scDotMessageRewritePrecedence :: ScDotMessage -> ScDotMessage
scDotMessageRewritePrecedence (ScDotMessage i a) =
  ScDotMessage i (map (scBasicExpressionRewritePrecedence True) a)

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
scTemporaryRewritePrecedence (i,e) = (i,fmap (scBasicExpressionRewritePrecedence True) e)

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
    ScPrimaryArrayExpression x -> ScPrimaryArrayExpression (map (scBasicExpressionRewritePrecedence True) x)
    ScPrimaryImplicitMessageSend x a -> ScPrimaryImplicitMessageSend x (map (scBasicExpressionRewritePrecedence True) a)

-- | Viewer for precedence rewriter. Reads, rewrites and prints Sc expression.
scRewritePrecedenceViewer :: String -> String
scRewritePrecedenceViewer =
  Sc.scInitializerDefinitionPrint .
  scInitializerDefinitionRewritePrecedence .
  Sc.superColliderParserInitializerDefinition .
  Sc.alexScanTokens

{-

rw = scRewritePrecedenceViewer
rw "p.q(a).r" == "(p.q(a)).r\n"
rw "p.q.r(a).s(b).t" == "(((p.q.r(a))).s(b)).t\n"
rw "p.q(a) + r" == "(p.q(a)) + r\n"
rw "p.q(a).r + s" == "(p.q(a)).r + s\n"
rw "p + q.r(a)" == "p + (q.r(a))\n"
rw "p.q(i)"== "p.q(i)\n"
rw "p.q(i).r" == "(p.q(i)).r\n"
rw "p.q(i).r(j)" == "(p.q(i)).r(j)\n"
rw "p.q + r" == "p.q + r\n" -- unary no parens
rw "p.q(i) + r" == "(p.q(i)) + r\n" -- parens ; singular requires if initial of binary, c.f. p.q()
rw "p + q.r(i)" == "p + (q.r(i))\n" -- parens ; also it subsequent of binary (ie. in binary requires paren)
rw "p.q(r + s.t(i))" == "p.q(r + (s.t(i)))\n" -- Binary within Unary/Nary
rw "p.q(r.s(i).t).u(j)" == "(p.q((r.s(i)).t)).u(j)\n"
rw "p.q(r.s(i).t).u(j) + k" == "((p.q((r.s(i)).t)).u(j)) + k\n"

rd = Sc.superColliderParserInitializerDefinition . Sc.alexScanTokens

-}
