{- | Precedence re-writing.
     The rewriting here is quite intricate.
     It goes to a little trouble to not introduce unnecessary parentheses.
-}
module Language.Smalltalk.Spl.Rewrite.Precedence where

import Language.Smalltalk.Spl.Ast {- stsc3 -}
import qualified Language.Smalltalk.Spl.Ast.Print as Spl {- stsc3 -}
import qualified Language.Smalltalk.Spl.Lexer as Spl {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Spl.Parser {- stsc3 -}

{- | This is for parenthesising.
     It places the initial nary selector at the end of the lhs, not the start of the rhs.
     If the rhs is null then the rewriting should halt (ie. null rhs is the recurrence check).
-}
splDotMessagesSplitAtNary :: [SplDotMessage] -> ([SplDotMessage], [SplDotMessage])
splDotMessagesSplitAtNary m =
  case break splDotMessageIsNary m of
    (lhs, k : rhs) -> (lhs ++ [k], rhs)
    _ -> error "splDotMessagesSplitAtNary?"

splPrimaryRewritePrecedenceMaybe :: Bool -> SplPrimary -> SplPrimary
splPrimaryRewritePrecedenceMaybe rw = if rw then splPrimaryRewritePrecedence else id

splDotMessageRewritePrecedenceMaybe :: Bool -> SplDotMessage -> SplDotMessage
splDotMessageRewritePrecedenceMaybe rw = if rw then splDotMessageRewritePrecedence else id

splBinaryMessageRewritePrecedenceMaybe :: Bool -> SplBinaryMessage -> SplBinaryMessage
splBinaryMessageRewritePrecedenceMaybe rw = if rw then splBinaryMessageRewritePrecedence else id

splBinaryArgumentRewritePrecedence :: SplBinaryArgument -> SplBinaryArgument
splBinaryArgumentRewritePrecedence (SplBinaryArgument p m) =
  case m of
    Nothing ->
      SplBinaryArgument (splPrimaryRewritePrecedence p) Nothing
    Just x ->
      if not (splDotMessagesHaveNary x)
        then
          SplBinaryArgument
            (splPrimaryRewritePrecedence p)
            (Just (map splDotMessageRewritePrecedence x))
        else
          let (lhs, rhs) = splDotMessagesSplitAtNary x
              rw = null rhs
              dmRw = splDotMessageRewritePrecedenceMaybe rw
          in (if rw then id else splBinaryArgumentRewritePrecedence)
              ( SplBinaryArgument
                  ( SplPrimaryExpression
                      ( SplExprBasic
                          ( SplBasicExpression
                              (splPrimaryRewritePrecedenceMaybe rw p)
                              (Just (SplMessagesDot (map dmRw lhs) Nothing))
                          )
                      )
                  )
                  (Just (map dmRw rhs))
              )

splBinaryMessageRewritePrecedence :: SplBinaryMessage -> SplBinaryMessage
splBinaryMessageRewritePrecedence (SplBinaryMessage i a) =
  SplBinaryMessage i (splBinaryArgumentRewritePrecedence a)

-- | Outer (initial) n-ary messages don't require parentheses.
splBasicExpressionRewritePrecedence :: Bool -> SplBasicExpression -> SplBasicExpression
splBasicExpressionRewritePrecedence isInit (SplBasicExpression p m) =
  case m of
    Nothing ->
      SplBasicExpression (splPrimaryRewritePrecedence p) Nothing
    Just (SplMessagesBinary b) ->
      SplBasicExpression
        (splPrimaryRewritePrecedence p)
        (Just (SplMessagesBinary (map splBinaryMessageRewritePrecedence b)))
    Just (SplMessagesDot d Nothing) ->
      if (isInit && length d == 1) || not (splDotMessagesHaveNary d)
        then
          SplBasicExpression
            (splPrimaryRewritePrecedence p)
            (Just (SplMessagesDot (map splDotMessageRewritePrecedence d) Nothing))
        else
          let (lhs, rhs) = splDotMessagesSplitAtNary d
              rw = null rhs
              dmRw = splDotMessageRewritePrecedenceMaybe rw
          in (if rw then id else splBasicExpressionRewritePrecedence isInit)
              ( SplBasicExpression
                  ( SplPrimaryExpression
                      ( SplExprBasic
                          ( SplBasicExpression
                              (splPrimaryRewritePrecedenceMaybe rw p)
                              (Just (SplMessagesDot (map dmRw lhs) Nothing))
                          )
                      )
                  )
                  (Just (SplMessagesDot (map dmRw rhs) Nothing))
              )
    Just (SplMessagesDot d (Just b)) ->
      if not (splDotMessagesHaveNary d)
        then
          SplBasicExpression
            (splPrimaryRewritePrecedence p)
            ( Just
                ( SplMessagesDot
                    (map splDotMessageRewritePrecedence d)
                    (Just (map splBinaryMessageRewritePrecedence b))
                )
            )
        else
          let (lhs, rhs) = splDotMessagesSplitAtNary d
              rw = null rhs
              dmRw = splDotMessageRewritePrecedenceMaybe rw
          in (if rw then id else splBasicExpressionRewritePrecedence False)
              ( SplBasicExpression
                  ( SplPrimaryExpression
                      ( SplExprBasic
                          ( SplBasicExpression
                              (splPrimaryRewritePrecedenceMaybe rw p)
                              (Just (SplMessagesDot (map dmRw lhs) Nothing))
                          )
                      )
                  )
                  ( Just
                      ( SplMessagesDot
                          (map dmRw rhs)
                          (Just (map (splBinaryMessageRewritePrecedenceMaybe rw) b))
                      )
                  )
              )

splExpressionRewritePrecedence :: SplExpression -> SplExpression
splExpressionRewritePrecedence e =
  case e of
    SplExprAssignment x y -> SplExprAssignment x (splExpressionRewritePrecedence y)
    SplExprBasic x -> SplExprBasic (splBasicExpressionRewritePrecedence True x)

splInitializerDefinitionRewritePrecedence :: SplInitializerDefinition -> SplInitializerDefinition
splInitializerDefinitionRewritePrecedence (SplInitializerDefinition cmt tmp stm) =
  SplInitializerDefinition
    cmt
    (fmap (map splTemporariesRewritePrecedence) tmp)
    (fmap splStatementsRewritePrecedence stm)

splDotMessageRewritePrecedence :: SplDotMessage -> SplDotMessage
splDotMessageRewritePrecedence (SplDotMessage i a) =
  SplDotMessage i (map (splBasicExpressionRewritePrecedence True) a)

splMessagesRewritePrecedence :: SplMessages -> SplMessages
splMessagesRewritePrecedence m =
  case m of
    SplMessagesDot m1 m2 ->
      SplMessagesDot
        (map splDotMessageRewritePrecedence m1)
        (fmap (map splBinaryMessageRewritePrecedence) m2)
    SplMessagesBinary m1 -> SplMessagesBinary (map splBinaryMessageRewritePrecedence m1)

splReturnStatementRewritePrecedence :: SplReturnStatement -> SplReturnStatement
splReturnStatementRewritePrecedence (SplReturnStatement x) =
  (SplReturnStatement (splExpressionRewritePrecedence x))

splStatementsRewritePrecedence :: SplStatements -> SplStatements
splStatementsRewritePrecedence s =
  case s of
    SplStatementsReturn x -> SplStatementsReturn (splReturnStatementRewritePrecedence x)
    SplStatementsExpression x y ->
      SplStatementsExpression
        (splExpressionRewritePrecedence x)
        (fmap splStatementsRewritePrecedence y)

splTemporaryRewritePrecedence :: SplTemporary -> SplTemporary
splTemporaryRewritePrecedence (i, e) = (i, fmap (splBasicExpressionRewritePrecedence True) e)

splTemporariesRewritePrecedence :: SplTemporaries -> SplTemporaries
splTemporariesRewritePrecedence = map splTemporaryRewritePrecedence

splBlockBodyRewritePrecedence :: SplBlockBody -> SplBlockBody
splBlockBodyRewritePrecedence (SplBlockBody a t s) =
  SplBlockBody a (fmap (map splTemporariesRewritePrecedence) t) (fmap splStatementsRewritePrecedence s)

splPrimaryRewritePrecedence :: SplPrimary -> SplPrimary
splPrimaryRewritePrecedence p =
  case p of
    SplPrimaryIdentifier _ -> p
    SplPrimaryLiteral _ -> p
    SplPrimaryBlock x -> SplPrimaryBlock (splBlockBodyRewritePrecedence x)
    SplPrimaryExpression x -> SplPrimaryExpression (splExpressionRewritePrecedence x)
    SplPrimaryArrayExpression x -> SplPrimaryArrayExpression (map (splBasicExpressionRewritePrecedence True) x)
    SplPrimaryDictionaryExpression x -> SplPrimaryDictionaryExpression (map (\(k, v) -> (k, splBasicExpressionRewritePrecedence True v)) x)
    SplPrimaryImplicitMessageSend x a -> SplPrimaryImplicitMessageSend x (map (splBasicExpressionRewritePrecedence True) a)

{-
-- | Viewer for precedence rewriter. Reads, rewrites and prints Sc expression.
splRewritePrecedenceViewer :: String -> String
splRewritePrecedenceViewer =
  Spl.splInitializerDefinitionPrint
    . splInitializerDefinitionRewritePrecedence
    . Spl.Parser.parseInitializerDefinition
    . Spl.alexScanTokens
-}

{-

rw = splRewritePrecedenceViewer
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

rd = Spl.superColliderParserInitializerDefinition . Spl.alexScanTokens

-}
