{- | Translate Sc Ast in relation to Smalltalk Ast.

The three main tasks are:

- changing the precedence rules
- packaging method parameters into a dictionary
- rewriting variable declarations

The SuperCollider message precedence rule is Dot > Binary.
The Smalltalk message precedence rule is Unary > Binary > Keyword.
A SuperCollider Dot message is either Unary or Keyword.
A Dot message sequence must have parentheses inserted after each Keyword element.

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

Method parameters must be collated into an array of associations.

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

-- | Parameter re-writing

scKeywordAssoc :: St.Keyword -> ScBasicExpression -> ScBasicExpression
scKeywordAssoc k v =
  let p = ScPrimaryLiteral (St.SelectorLiteral (St.KeywordSelector k))
      rhs = ScPrimaryExpression (ScExprBasic v)
      m = ScMessagesBinary [ScBinaryMessage "->" (ScBinaryArgument rhs Nothing)]
  in ScBasicExpression p (Just m)

scKeywordArgumentAssoc :: ScKeywordArgument -> ScBasicExpression
scKeywordArgumentAssoc (ScKeywordArgument k v) =
  case k of
    Just x -> scKeywordAssoc x v
    Nothing -> v

scKeywordArgumentsArray :: [ScKeywordArgument] -> ScBasicExpression
scKeywordArgumentsArray a =
  ScBasicExpression (ScPrimaryArrayExpression (map scKeywordArgumentAssoc a)) Nothing

-- | Rewrite Keyword messages to have a single association array as argument.
scDotMessageRewriteKeyword :: ScDotMessage -> ScDotMessage
scDotMessageRewriteKeyword (ScDotMessage m a) =
  case a of
    Nothing -> ScDotMessage m a
    Just p -> ScDotMessage m (Just [ScKeywordArgument Nothing (scKeywordArgumentsArray p)])

-- * Precedence re-writing

scDotMessageIsKeyword :: ScDotMessage -> Bool
scDotMessageIsKeyword (ScDotMessage _ m) = isJust m

scDotMessagesHaveKeyword :: [ScDotMessage] -> Bool
scDotMessagesHaveKeyword = any scDotMessageIsKeyword

scDotMessagesSplitAtKeyword :: [ScDotMessage] -> ([ScDotMessage], [ScDotMessage])
scDotMessagesSplitAtKeyword m =
  case break scDotMessageIsKeyword m of
    (lhs,k:rhs) -> (lhs ++ [k],rhs)
    _ -> error "scDotMessagesSplitAtKeyword?"

scBinaryArgumentRewritePrecedence :: ScBinaryArgument -> ScBinaryArgument
scBinaryArgumentRewritePrecedence (ScBinaryArgument p m) =
  case m of
    Nothing -> ScBinaryArgument p m
    Just x -> if not (scDotMessagesHaveKeyword x)
              then ScBinaryArgument p m
              else let (lhs,rhs) = scDotMessagesSplitAtKeyword x
                   in scBinaryArgumentRewritePrecedence
                      (ScBinaryArgument
                       (ScPrimaryExpression
                         (ScExprBasic
                           (ScBasicExpression p (Just (ScMessagesDot lhs Nothing)))))
                       (Just rhs))

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
      else scBasicExpressionRewritePrecedence
           (ScBasicExpression
            (ScPrimaryExpression
             (ScExprBasic
              (ScBasicExpression p (Just (ScMessagesDot d Nothing)))))
            (Just (ScMessagesBinary b)))

scExpressionRewritePrecedence :: ScExpression -> ScExpression
scExpressionRewritePrecedence e =
  case e of
    ScExprAssignment x y -> ScExprAssignment x (scExpressionRewritePrecedence y)
    ScExprBasic x -> ScExprBasic (scBasicExpressionRewritePrecedence x)

scPrimaryRewritePrecedence :: ScPrimary -> ScPrimary
scPrimaryRewritePrecedence p =
  case p of
    ScPrimaryIdentifier _ -> p
    ScPrimaryLiteral _ -> p
    -- the block needs to be rewritten for precedence also
    ScPrimaryBlock x -> ScPrimaryBlock (scBlockBodyRewriteTemporaries x)
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
      in case tmpExpr of
        [] -> ScBlockBody arg maybeTmp stm
        _ -> ScBlockBody arg maybeTmp (Just (scExpressionSequenceToStatements stm tmpExpr))

{-
import Language.Smalltalk.SuperCollider.Ast.Print
import Language.Smalltalk.SuperCollider.Lexer
import Language.Smalltalk.SuperCollider.Parser
p = superColliderParser . alexScanTokens
x = scExpressionPrint . p
x "p.q + r"
x "p + q.r(a)"
rw = scExpressionPrint . scExpressionRewritePrecedence . p
rw "p + q.r(a)" --  p + (q.r(a))
rw "p + q.r.s(a)"
rw "p + q.r.s(a).t"
rw "p + q.r.s(a).t.u(b)"
rw "p.q(a)" -- only one message, can be keyword
rw "p.q(a).r"
rw "p.q(a).r(b)" -- hence nested trailing
rw "p + q"
rw "p.q + r" -- unary no parens
rw "p.q(a) + r" -- parens ; singular requires if initial of binary, c.f. p.q(a)
rw "p.q(a) + r.s(b)"
rw "{var x = a; x}"
rw "{var x = a, y; x + y}"
rw "{var x; var y = b; x + y}"
-}
