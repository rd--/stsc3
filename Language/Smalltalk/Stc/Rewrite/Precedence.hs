{- | Precedence re-writing.
     The rewriting here is quite intricate.
     It goes to a little trouble to not introduce unnecessary parentheses.
-}
module Language.Smalltalk.Stc.Rewrite.Precedence where

import Language.Smalltalk.Stc.Ast {- stsc3 -}
import qualified Language.Smalltalk.Stc.Ast.Print as Stc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Lexer as Stc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Parser as Stc {- stsc3 -}

{- | This is for parenthesising.
     It places the initial nary selector at the end of the lhs, not the start of the rhs.
     If the rhs is null then the rewriting should halt (ie. null rhs is the recurrence check).
-}
stcDotMessagesSplitAtNary :: [StcDotMessage] -> ([StcDotMessage], [StcDotMessage])
stcDotMessagesSplitAtNary m =
  case break stcDotMessageIsNary m of
    (lhs, k : rhs) -> (lhs ++ [k], rhs)
    _ -> error "stcDotMessagesSplitAtNary?"

stcPrimaryRewritePrecedenceMaybe :: Bool -> StcPrimary -> StcPrimary
stcPrimaryRewritePrecedenceMaybe rw = if rw then stcPrimaryRewritePrecedence else id

stcDotMessageRewritePrecedenceMaybe :: Bool -> StcDotMessage -> StcDotMessage
stcDotMessageRewritePrecedenceMaybe rw = if rw then stcDotMessageRewritePrecedence else id

stcBinaryMessageRewritePrecedenceMaybe :: Bool -> StcBinaryMessage -> StcBinaryMessage
stcBinaryMessageRewritePrecedenceMaybe rw = if rw then stcBinaryMessageRewritePrecedence else id

stcBinaryArgumentRewritePrecedence :: StcBinaryArgument -> StcBinaryArgument
stcBinaryArgumentRewritePrecedence (StcBinaryArgument p m) =
  case m of
    Nothing ->
      StcBinaryArgument (stcPrimaryRewritePrecedence p) Nothing
    Just x ->
      if not (stcDotMessagesHaveNary x)
        then
          StcBinaryArgument
            (stcPrimaryRewritePrecedence p)
            (Just (map stcDotMessageRewritePrecedence x))
        else
          let (lhs, rhs) = stcDotMessagesSplitAtNary x
              rw = null rhs
              dmRw = stcDotMessageRewritePrecedenceMaybe rw
          in (if rw then id else stcBinaryArgumentRewritePrecedence)
              ( StcBinaryArgument
                  ( StcPrimaryExpression
                      ( StcExprBasic
                          ( StcBasicExpression
                              (stcPrimaryRewritePrecedenceMaybe rw p)
                              (Just (StcMessagesDot (map dmRw lhs) Nothing))
                          )
                      )
                  )
                  (Just (map dmRw rhs))
              )

stcBinaryMessageRewritePrecedence :: StcBinaryMessage -> StcBinaryMessage
stcBinaryMessageRewritePrecedence (StcBinaryMessage i a) =
  StcBinaryMessage i (stcBinaryArgumentRewritePrecedence a)

-- | Outer (initial) n-ary messages don't require parentheses.
stcBasicExpressionRewritePrecedence :: Bool -> StcBasicExpression -> StcBasicExpression
stcBasicExpressionRewritePrecedence isInit (StcBasicExpression p m) =
  case m of
    Nothing ->
      StcBasicExpression (stcPrimaryRewritePrecedence p) Nothing
    Just (StcMessagesBinary b) ->
      StcBasicExpression
        (stcPrimaryRewritePrecedence p)
        (Just (StcMessagesBinary (map stcBinaryMessageRewritePrecedence b)))
    Just (StcMessagesDot d Nothing) ->
      if (isInit && length d == 1) || not (stcDotMessagesHaveNary d)
        then
          StcBasicExpression
            (stcPrimaryRewritePrecedence p)
            (Just (StcMessagesDot (map stcDotMessageRewritePrecedence d) Nothing))
        else
          let (lhs, rhs) = stcDotMessagesSplitAtNary d
              rw = null rhs
              dmRw = stcDotMessageRewritePrecedenceMaybe rw
          in (if rw then id else stcBasicExpressionRewritePrecedence isInit)
              ( StcBasicExpression
                  ( StcPrimaryExpression
                      ( StcExprBasic
                          ( StcBasicExpression
                              (stcPrimaryRewritePrecedenceMaybe rw p)
                              (Just (StcMessagesDot (map dmRw lhs) Nothing))
                          )
                      )
                  )
                  (Just (StcMessagesDot (map dmRw rhs) Nothing))
              )
    Just (StcMessagesDot d (Just b)) ->
      if not (stcDotMessagesHaveNary d)
        then
          StcBasicExpression
            (stcPrimaryRewritePrecedence p)
            ( Just
                ( StcMessagesDot
                    (map stcDotMessageRewritePrecedence d)
                    (Just (map stcBinaryMessageRewritePrecedence b))
                )
            )
        else
          let (lhs, rhs) = stcDotMessagesSplitAtNary d
              rw = null rhs
              dmRw = stcDotMessageRewritePrecedenceMaybe rw
          in (if rw then id else stcBasicExpressionRewritePrecedence False)
              ( StcBasicExpression
                  ( StcPrimaryExpression
                      ( StcExprBasic
                          ( StcBasicExpression
                              (stcPrimaryRewritePrecedenceMaybe rw p)
                              (Just (StcMessagesDot (map dmRw lhs) Nothing))
                          )
                      )
                  )
                  ( Just
                      ( StcMessagesDot
                          (map dmRw rhs)
                          (Just (map (stcBinaryMessageRewritePrecedenceMaybe rw) b))
                      )
                  )
              )

stcExpressionRewritePrecedence :: StcExpression -> StcExpression
stcExpressionRewritePrecedence e =
  case e of
    StcExprAssignment x y -> StcExprAssignment x (stcExpressionRewritePrecedence y)
    StcExprBasic x -> StcExprBasic (stcBasicExpressionRewritePrecedence True x)

stcInitializerDefinitionRewritePrecedence :: StcInitializerDefinition -> StcInitializerDefinition
stcInitializerDefinitionRewritePrecedence (StcInitializerDefinition cmt tmp stm) =
  StcInitializerDefinition
    cmt
    (fmap (map stcTemporariesRewritePrecedence) tmp)
    (fmap stcStatementsRewritePrecedence stm)

stcDotMessageRewritePrecedence :: StcDotMessage -> StcDotMessage
stcDotMessageRewritePrecedence (StcDotMessage i a) =
  StcDotMessage i (map (stcBasicExpressionRewritePrecedence True) a)

stcMessagesRewritePrecedence :: StcMessages -> StcMessages
stcMessagesRewritePrecedence m =
  case m of
    StcMessagesDot m1 m2 ->
      StcMessagesDot
        (map stcDotMessageRewritePrecedence m1)
        (fmap (map stcBinaryMessageRewritePrecedence) m2)
    StcMessagesBinary m1 -> StcMessagesBinary (map stcBinaryMessageRewritePrecedence m1)

stcReturnStatementRewritePrecedence :: StcReturnStatement -> StcReturnStatement
stcReturnStatementRewritePrecedence (StcReturnStatement x) =
  (StcReturnStatement (stcExpressionRewritePrecedence x))

stcStatementsRewritePrecedence :: StcStatements -> StcStatements
stcStatementsRewritePrecedence s =
  case s of
    StcStatementsReturn x -> StcStatementsReturn (stcReturnStatementRewritePrecedence x)
    StcStatementsExpression x y ->
      StcStatementsExpression
        (stcExpressionRewritePrecedence x)
        (fmap stcStatementsRewritePrecedence y)

stcTemporaryRewritePrecedence :: StcTemporary -> StcTemporary
stcTemporaryRewritePrecedence (i, e) = (i, fmap (stcBasicExpressionRewritePrecedence True) e)

stcTemporariesRewritePrecedence :: StcTemporaries -> StcTemporaries
stcTemporariesRewritePrecedence = map stcTemporaryRewritePrecedence

stcBlockBodyRewritePrecedence :: StcBlockBody -> StcBlockBody
stcBlockBodyRewritePrecedence (StcBlockBody a t s) =
  StcBlockBody a (fmap (map stcTemporariesRewritePrecedence) t) (fmap stcStatementsRewritePrecedence s)

stcPrimaryRewritePrecedence :: StcPrimary -> StcPrimary
stcPrimaryRewritePrecedence p =
  case p of
    StcPrimaryIdentifier _ -> p
    StcPrimaryLiteral _ -> p
    StcPrimaryBlock x -> StcPrimaryBlock (stcBlockBodyRewritePrecedence x)
    StcPrimaryExpression x -> StcPrimaryExpression (stcExpressionRewritePrecedence x)
    StcPrimaryArrayExpression x -> StcPrimaryArrayExpression (map (stcBasicExpressionRewritePrecedence True) x)
    StcPrimaryDictionaryExpression x -> StcPrimaryDictionaryExpression (map (\(k, v) -> (k, stcBasicExpressionRewritePrecedence True v)) x)
    StcPrimaryImplicitMessageSend x a -> StcPrimaryImplicitMessageSend x (map (stcBasicExpressionRewritePrecedence True) a)

-- | Viewer for precedence rewriter. Reads, rewrites and prints Sc expression.
stcRewritePrecedenceViewer :: String -> String
stcRewritePrecedenceViewer =
  Stc.stcInitializerDefinitionPrint
    . stcInitializerDefinitionRewritePrecedence
    . Stc.parseInitializerDefinition
    . Stc.alexScanTokens

{-

rw = stcRewritePrecedenceViewer
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

rd = Stc.superColliderParserInitializerDefinition . Stc.alexScanTokens

-}
