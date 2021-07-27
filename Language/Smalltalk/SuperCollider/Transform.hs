{- | Rewrite Sc Ast to conform to Smalltalk Ast rules.

The three main tasks are:

- changing the precedence rules
- packaging method parameters into a dictionary (array of associations)
- rewriting variable declarations

The SuperCollider message precedence rule is Dot > Binary.
The Smalltalk message precedence rule is Unary > Binary > Keyword.
A SuperCollider Dot message is either Unary or Keyword.
To translate, a Dot message sequence has parentheses inserted after each Keyword element.

- p.q(a).r          => (p q: a) r
- p.q.r(a).s(b).t   => ((p q r: a) s: b) t
- p.q(a) + r        => (p q: a) + r
- p.q(a).r + s      => (p q: a) r + s
- p + q.r(a)        => p + (q r: a)

This transformation can be done at the Sc Ast.

- p.q(a).r          => (p.q(a)).r
- p.q.r(a).s(b).t   => ((p.q.r(a)).s(b)) t
- p.q(a) + r        => (p.q(a)) + r
- p.q(a).r + s      => (p.q(a)).r + s
- p + q.r(a)        => p + (q.r(a))

Method parameters are collated into an array of associations.

- p.q(a,b)          => p q: {a. b}
- p.q(x: a, b)      => p q: {#x: -> a. b}

This transformation can also be done at the Sc Ast.

- p.q(a,b)          => p.q([a,b])
- p.q(x: a, b)      => p.q([\x: -> a,b])

Temporary variables in SuperCollider may have initialisers and there may be multiple sets.
These must be rewritten to as a single set of names and a sequence of assignments.

- var p, q = a;     => |p q| q := a.
- var p=a; var q;   => |p q| p := a.

This transformation can also be done at the Sc Ast.

- var p, q = a;     => var p, q; q = a;
- var p = a; var q; => var p, q; p = a;

-}
module Language.Smalltalk.SuperCollider.Transform where

import Data.Maybe {- maybe -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}

scExpressionRewrite :: ScExpression -> ScExpression
scExpressionRewrite =
  scExpressionRewritePrecedence .
  scExpressionRewriteTemporaries .
  scExpressionRewriteKeyword

-- * Parameter re-writing

scKeywordAssoc :: St.Keyword -> ScBasicExpression -> ScBasicExpression
scKeywordAssoc k v =
  let p = ScPrimaryLiteral (St.SelectorLiteral (St.KeywordSelector k))
      rhs = ScPrimaryExpression (ScExprBasic v)
      m = ScMessagesBinary [ScBinaryMessage "->" (ScBinaryArgument rhs Nothing)]
  in ScBasicExpression p (Just m)

scKeywordArgumentAssoc :: ScKeywordArgument -> ScBasicExpression
scKeywordArgumentAssoc (ScKeywordArgument k v) =
  case k of
    Just x -> scKeywordAssoc x (scBasicExpressionRewriteKeyword v)
    Nothing -> scBasicExpressionRewriteKeyword v

scKeywordArgumentsArray :: [ScKeywordArgument] -> ScBasicExpression
scKeywordArgumentsArray a =
  ScBasicExpression (ScPrimaryArrayExpression (map scKeywordArgumentAssoc a)) Nothing

{- | Rewrite Keyword messages to have a single association array as argument.
     ScDotMessage occurs in ScMessages and in ScBinaryArgument, which occurs in ScMessages.
     ScMessages occurs in ScBasicExpression.
-}
scKeywordArgumentsRewriteKeyword :: [ScKeywordArgument] -> [ScKeywordArgument]
scKeywordArgumentsRewriteKeyword p =
  [ScKeywordArgument Nothing (scKeywordArgumentsArray p)]

scDotMessageRewriteKeyword :: ScDotMessage -> ScDotMessage
scDotMessageRewriteKeyword (ScDotMessage m a) =
  ScDotMessage m (fmap scKeywordArgumentsRewriteKeyword a)

scExpressionRewriteKeyword :: ScExpression -> ScExpression
scExpressionRewriteKeyword e =
  case e of
    ScExprAssignment x y -> ScExprAssignment x (scExpressionRewriteKeyword y)
    ScExprBasic x -> ScExprBasic (scBasicExpressionRewriteKeyword x)

scTemporaryRewriteKeyword :: ScTemporary -> ScTemporary
scTemporaryRewriteKeyword (i,e) = (i,fmap scBasicExpressionRewriteKeyword e)

scTemporariesRewriteKeyword :: ScTemporaries -> ScTemporaries
scTemporariesRewriteKeyword = map scTemporaryRewriteKeyword

scReturnStatementRewriteKeyword :: ScReturnStatement -> ScReturnStatement
scReturnStatementRewriteKeyword (ScReturnStatement x) =
  (ScReturnStatement (scExpressionRewriteKeyword x))

scStatementsRewriteKeyword :: ScStatements -> ScStatements
scStatementsRewriteKeyword s =
  case s of
    ScStatementsReturn x -> ScStatementsReturn (scReturnStatementRewriteKeyword x)
    ScStatementsExpression x y -> ScStatementsExpression
                                  (scExpressionRewriteKeyword x)
                                  (fmap scStatementsRewriteKeyword y)

scBlockBodyRewriteKeyword :: ScBlockBody -> ScBlockBody
scBlockBodyRewriteKeyword (ScBlockBody a t s) =
  ScBlockBody a (fmap (map scTemporariesRewriteKeyword) t) (fmap scStatementsRewriteKeyword s)

scBasicExpressionRewriteKeyword :: ScBasicExpression -> ScBasicExpression
scBasicExpressionRewriteKeyword (ScBasicExpression p m) =
  ScBasicExpression (scPrimaryRewriteKeyword p) (fmap scMessagesRewriteKeyword m)

scPrimaryRewriteKeyword :: ScPrimary -> ScPrimary
scPrimaryRewriteKeyword p =
  case p of
    ScPrimaryIdentifier _ -> p
    ScPrimaryLiteral _ -> p
    ScPrimaryBlock x -> ScPrimaryBlock (scBlockBodyRewriteKeyword x)
    ScPrimaryExpression x -> ScPrimaryExpression (scExpressionRewriteKeyword x)
    ScPrimaryArrayExpression x -> ScPrimaryArrayExpression (map scBasicExpressionRewriteKeyword x)

scBinaryArgumentRewriteKeyword :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewriteKeyword (ScBinaryArgument p m) =
  ScBinaryArgument (scPrimaryRewriteKeyword p) (fmap (map scDotMessageRewriteKeyword) m)

scBinaryMessageRewriteKeyword :: ScBinaryMessage -> ScBinaryMessage
scBinaryMessageRewriteKeyword (ScBinaryMessage i a) =
  ScBinaryMessage i (scBinaryArgumentRewriteKeyword a)

scMessagesRewriteKeyword :: ScMessages -> ScMessages
scMessagesRewriteKeyword m =
  case m of
    ScMessagesDot m1 m2 -> ScMessagesDot
                           (map scDotMessageRewriteKeyword m1)
                           (fmap (map scBinaryMessageRewriteKeyword) m2)
    ScMessagesBinary m1 -> ScMessagesBinary (map scBinaryMessageRewriteKeyword m1)

-- * Precedence re-writing

scDotMessageIsKeyword :: ScDotMessage -> Bool
scDotMessageIsKeyword (ScDotMessage _ m) = isJust m

scDotMessagesHaveKeyword :: [ScDotMessage] -> Bool
scDotMessagesHaveKeyword = any scDotMessageIsKeyword

-- | This is for parenthesising, it places the initial keyword at the end of the lhs, not the start of the rhs.
scDotMessagesSplitAtKeyword :: [ScDotMessage] -> ([ScDotMessage], [ScDotMessage])
scDotMessagesSplitAtKeyword m =
  case break scDotMessageIsKeyword m of
    (lhs,k:rhs) -> (lhs ++ [k],rhs)
    _ -> error "scDotMessagesSplitAtKeyword?"

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

-- | The recursion rules here are intricate and likely incorrect.
scBinaryArgumentRewritePrecedence :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewritePrecedence (ScBinaryArgument p m) =
  case m of
    Nothing -> ScBinaryArgument (scPrimaryRewritePrecedence p) Nothing
    Just x -> if not (scDotMessagesHaveKeyword x)
              then ScBinaryArgument (scPrimaryRewritePrecedence p) m
              else let (lhs,rhs) = scDotMessagesSplitAtKeyword x
                   in --scBinaryArgumentRewritePrecedence
                      (ScBinaryArgument
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
      then ScBasicExpression p m
      else let (lhs,rhs) = scDotMessagesSplitAtKeyword d
           in scBasicExpressionRewritePrecedence
              (ScBasicExpression
                (ScPrimaryExpression
                  (ScExprBasic
                    (ScBasicExpression p (Just (ScMessagesDot lhs Nothing)))))
                (Just (ScMessagesDot rhs Nothing)))
    Just (ScMessagesDot d (Just b)) ->
      if not (scDotMessagesHaveKeyword d)
      then ScBasicExpression
           (scPrimaryRewritePrecedence p)
           (Just (ScMessagesDot d (Just (map scBinaryMessageRewritePrecedence b))))
      else --scBasicExpressionRewritePrecedence -- too many paren?
           (ScBasicExpression
            (ScPrimaryExpression
             (ScExprBasic
              (ScBasicExpression
                (scPrimaryRewritePrecedence p)
                (Just (ScMessagesDot d Nothing)))))
            (Just (scMessagesRewritePrecedence (ScMessagesBinary b))))

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

-- * Rewrite Temporaries

scTemporariesRewrite :: [ScTemporaries] -> ([St.Identifier], [ScExpression])
scTemporariesRewrite tmp =
  let tmpNames = map fst (concat tmp)
      tmpExpr = mapMaybe (\(k,v) -> fmap (\e -> ScExprAssignment k (ScExprBasic e)) v) (concat tmp)
  in (tmpNames,tmpExpr)

scBlockBodyRewriteTemporaries :: ScBlockBody -> ScBlockBody
scBlockBodyRewriteTemporaries (ScBlockBody arg tmpMaybe stm) =
  case tmpMaybe of
    Nothing -> ScBlockBody arg Nothing stm
    Just tmp ->
      let (tmpNames,tmpExpr) = scTemporariesRewrite tmp
          idToTmp k = (k,Nothing)
          maybeTmp = if null tmpNames then Nothing else Just [map idToTmp tmpNames]
          tmpExprRw = map scExpressionRewriteTemporaries tmpExpr
      in case tmpExpr of
        [] -> ScBlockBody arg maybeTmp stm
        _ -> ScBlockBody arg maybeTmp (Just (scExpressionSequenceToStatements stm tmpExprRw))

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

{-

import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc
import qualified Language.Smalltalk.SuperCollider.Parser as Sc

rw = Sc.scExpressionPrint . scExpressionRewrite . Sc.superColliderParser . Sc.alexScanTokens
rw "p + q.r()" == "p + (q.r([]))"

rw = Sc.scExpressionPrint . scExpressionRewritePrecedence . Sc.superColliderParser . Sc.alexScanTokens
rw "p.q + r" == "p.q + r"
rw "p + q.r()" == "p + (q.r())"
rw "p + q.r(x + y.z())" == "p + (q.r(x + (y.z())))"
rw "p + q + r.s()" == "p + q + (r.s())"
rw "p + q + r.s() + t" == "p + q + (r.s()) + t"
rw "p + (q + r.s())" == "p + (q + (r.s()))"
rw "p + q.r.s()" == "p + (q.r.s())"
rw "p + q.r.s().t" == "p + (q.r.s()).t"
rw "p + q.r.s().t.u()" == "p + (q.r.s()).t.u()"
rw "p.q()" == "p.q()" -- only one message, can be keyword
rw "p.q().r" == "(p.q()).r"
rw "p.q().r()" == "(p.q()).r()" -- hence nested trailing
rw "p + q" == "p + q"
rw "p.q + r" == "p.q + r" -- unary no parens
rw "p.q() + r" == "(p.q()) + r" -- parens ; singular requires if initial of binary, c.f. p.q(a)
rw "p.q() + r.s()" == "(p.q()) + (r.s())"
rw "p + q + r.s()" == "p + q + (r.s())"
rw "{p.q(a).r}" == "{(p.q(a)).r}"
rw "{var x = p.q(a).r; x}" == "{var x = (p.q(a)).r; x}"
rw "1 + p.q(x: a + r.s())" == "1 + (p.q(x:a + (r.s())))"

rw = Sc.scExpressionPrint . scExpressionRewriteTemporaries . Sc.superColliderParser . Sc.alexScanTokens
rw "{var x = a; x}" == "{var x; x = a; x}"
rw "{var x = a,y; x + y}" == "{var x,y; x = a; x + y}"
rw "{var x; var y = b; x + y}" == "{var x,y; y = b; x + y}"
rw "{var x = {var y = a; a * x}.value; x}" == "{var x; x = {var y; y = a; a * x}.value; x}"

rw = Sc.scExpressionPrint . scExpressionRewriteKeyword . Sc.superColliderParser . Sc.alexScanTokens
rw "p.q()" == "p.q([])"
rw "p.q(a)" == "p.q([a])"
rw "p.q(a,b)" == "p.q([a,b])"
rw "p.q(x:a,b)" == "p.q([\\x: -> (a),b])"
rw "p.q(a,x:b)" == "p.q([a,\\x: -> (b)])"
rw "p.q(a,x: b.c(y: d,e))" == "p.q([a,\\x: -> (b.c([\\y: -> (d),e]))])"

-}
