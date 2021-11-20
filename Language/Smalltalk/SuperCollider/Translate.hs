{- | Translate from the SuperCollider (Sc) Ast to the Smalltalk (St) Ast.
     Requires that the Sc Ast has been rewritten to have only constructs that can be translated to St.
     See "Language.Smalltalk.SuperCollider.Rewrite"
-}
module Language.Smalltalk.SuperCollider.Translate where

import qualified Data.List.Split as Split {- split -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr as Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr.Print as Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as Print {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Lexer as Lexer {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Parser {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Rewrite as Rewrite {- stsc3 -}

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
         r -> error ("scDotMessagesForSmalltalk: " ++ show (m, r))
  else (m,Nothing)

scFloatConstant :: St.Identifier -> St.Primary
scFloatConstant x =
  St.PrimaryExpression
  (St.ExprBasic
    (St.BasicExpression
      (St.PrimaryIdentifier "Float")
      (Just (St.MessagesUnary [St.UnaryMessage x] Nothing Nothing))
      Nothing))

-- | The Sc implicit message send x(...) is translated as (x apply: {...})
scImplictMessageSendSt :: St.Identifier -> [ScBasicExpression] -> St.Primary
scImplictMessageSendSt x a =
  St.PrimaryExpression
  (St.ExprBasic
    (St.BasicExpression
      (St.PrimaryIdentifier x)
      (Just (St.MessagesKeyword
              (St.KeywordMessage
                [("apply:"
                 ,St.KeywordArgument (St.PrimaryArrayExpression (map scBasicExpressionSt a)) Nothing Nothing)])))
      Nothing))

scPrimarySt :: ScPrimary -> St.Primary
scPrimarySt p =
  case p of
    ScPrimaryIdentifier x ->
        case x of
          "this" -> St.PrimaryIdentifier "self"
          "pi" -> scFloatConstant "pi"
          "inf" -> scFloatConstant "infinity"
          _ -> St.PrimaryIdentifier x
    ScPrimaryLiteral x -> St.PrimaryLiteral x
    ScPrimaryBlock x -> St.PrimaryBlock (scBlockBodySt x)
    ScPrimaryExpression x -> St.PrimaryExpression (scExpressionSt x)
    ScPrimaryArrayExpression x -> St.PrimaryArrayExpression (map scBasicExpressionSt x)
    ScPrimaryImplictMessageSend x a -> scImplictMessageSendSt x a

scBlockBodySt :: ScBlockBody -> St.BlockBody
scBlockBodySt (ScBlockBody arg tmp stm) =
  St.BlockBody Nothing arg (fmap scTemporariesSt tmp) (fmap scStatementsSt stm)

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
               (St.basicExpressionToPrimary (scBasicExpressionSt e))
               Nothing
               Nothing

{- | For .stc to .st translation message names are allowed to have interior colons.
     This means "c at: i put: i" can be written as "c.at:put(i, j)".
     In addition "r.value(i, j)" is understood to mean "r value: i value: j".
     That is, if the message name has fewer parts than there are arguments, supply the implicit name "value" for the missing parts.
-}
scDotMessageKeywordSt :: ScDotMessage -> St.KeywordMessage
scDotMessageKeywordSt (ScDotMessage nm arg) =
  let extendWithValue = True
      nmParts = Split.splitOn ":" nm
      nmSize = length nmParts
      argSize = length arg
      messageParts = if extendWithValue && nmSize < argSize
                     then take argSize (nmParts ++ repeat "value")
                     else nmParts
  in if null arg
     then error "scDotMessageKeywordSt: Unary"
     else if length messageParts == argSize
          then St.KeywordMessage (zipWith (\i j -> (i ++ ":",scKeywordArgumentSt j)) messageParts arg)
          else error "scDotMessageKeywordSt: N-ary messages name degree mismatch"

scDotMessageUnarySt :: ScDotMessage -> St.UnaryMessage
scDotMessageUnarySt (ScDotMessage i a) =
  case a of
    [] -> St.UnaryMessage i
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

scInitializerDefinitionSt :: ScInitializerDefinition -> St.InitializerDefinition
scInitializerDefinitionSt (ScInitializerDefinition tmp stm) =
   St.InitializerDefinition (fmap scTemporariesSt tmp) (fmap scStatementsSt stm)

-- * SuperCollider to Smalltalk translator

-- | Parse and translate SuperCollider InitializerDefinition.
scParseInitializerDefinition :: String -> St.InitializerDefinition
scParseInitializerDefinition s =
  let eSc = Parser.superColliderParser (Lexer.alexScanTokens s)
  in scInitializerDefinitionSt (Rewrite.scInitializerDefinitionRewrite True eSc)

-- | Translate SuperCollider program text to Smalltalk.
scToSt :: String -> String
scToSt = Print.initializerDefinition_pp . scParseInitializerDefinition

-- * C-Smalltalk translator

-- | Parse C-Smalltalk InitializerDefinition.
stcParseInitializerDefinition :: String -> St.InitializerDefinition
stcParseInitializerDefinition s =
  let eSc = Parser.superColliderParser (Lexer.alexScanTokens s)
  in scInitializerDefinitionSt (Rewrite.scInitializerDefinitionRewrite False eSc)

-- | Translate C-Smalltalk program text to Smalltalk.
stcToSt :: String -> String
stcToSt = Print.initializerDefinition_pp . stcParseInitializerDefinition

-- > stcToExprToStc "(a - b).c"
stcToExprToStc :: String -> [String]
stcToExprToStc =
  map Expr.exprPrintStc .
  Expr.initStatements .
  Expr.initializerDefinitionExpr .
  stcParseInitializerDefinition

{-

rw = stcToSt
rw "(p - q).m" == "(p - q) m .\n"
rw "p(q.r(i).s).t(j) + k"
rw "SinOsc(220,0.5)" == "(SinOsc apply: {220 . 0.5}) .\n"
rw "x(i) + y(j,k)" == "(x apply: {i}) + (y apply: {j . k}) .\n"
rw "x(i, y(j, k))" == "(x apply: {i . (y apply: {j . k})}) .\n"
rw "p + q.r.s(a).t.u(b)" == "p + (((q r s: a)) t u: b) .\n"
rw "p + q.r + s.t(u) + v()" == "p + q r + (s t: u) + (v apply: {}) .\n"
rw "p.q:r(i,j)" == "p q: i r: j .\n"
rw "x + p.at:put(x, y)" == "x + (p at: x put: y) .\n"
rw "f.value(i, j)" == "f value: i value: j .\n"
rw "c.put(i, j)" == "c put: i value: j .\n"
rw "p.q:r(i)" -- error ; arity mismatch
rw "p.q:(i)" -- error ; message names may not have trailing colons
rw "p.q(x: i)" -- error ; keywords are not allowed
rw "p(q.r(i).s).t + k" -- "(p apply: {(q r: i) s}) t + k .\n"

rw = scToSt
scToSt "p.q(r: i)" == "p q: {#'r:' -> i} .\n"
scToSt "p.q:r(i)" -- error ; this is an error in Sc and is reported here but with an odd message
scToSt "p.q()" -- error ; this was dis-allowed for stcToSt ; it's not neccesary in Sc but it is allowed

-}
