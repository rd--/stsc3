{- | Rewrite Sc Ast to conform to Smalltalk Ast rules.

The three main tasks are:

- changing the precedence rules
- packaging method parameters into a dictionary (array of associations)
- rewriting variable declarations

The SuperCollider message precedence rule is Dot > Binary.
The Smalltalk message precedence rule is Unary > Binary > Keyword.
A SuperCollider Dot message is either Unary or Keyword.
To translate, a Dot message sequence has parentheses inserted after each Keyword element.

- p.q(a).r          => (p q: a) r                  <= p q: a r   ==> p q: (a r)
- p.q.r(a).s(b).t   => ((p q r: a) s: b) t
- p.q(a) + r        => (p q: a) + r
- p.q(a).r + s      => (p q: a) r + s
- p + q.r(a)        => p + (q r: a)                <= p + q r: a ==> (p + q) r: a

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
module Language.Smalltalk.SuperCollider.Rewrite where

import Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import Language.Smalltalk.SuperCollider.Rewrite.Keyword
import Language.Smalltalk.SuperCollider.Rewrite.Precedence
import Language.Smalltalk.SuperCollider.Rewrite.Temporaries

scExpressionRewrite :: ScExpression -> ScExpression
scExpressionRewrite =
  scExpressionRewritePrecedence .
  scExpressionRewriteTemporaries .
  scExpressionRewriteKeyword

{-

import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc
import qualified Language.Smalltalk.SuperCollider.Parser as Sc

rw = Sc.scExpressionPrint . scExpressionRewrite . Sc.superColliderParser . Sc.alexScanTokens

rw = Sc.scExpressionPrint . scExpressionRewritePrecedence . Sc.superColliderParser . Sc.alexScanTokens
rw "p.q()" == "p.q()" -- only one message, can be keyword
rw "p.q().r()" == "(p.q()).r()" -- nested trailing
rw "p.q + r" == "p.q + r" -- unary no parens
rw "p.q() + r" == "(p.q()) + r" -- parens ; singular requires if initial of binary, c.f. p.q(a)

rw = Sc.scExpressionPrint . scExpressionRewriteTemporaries . Sc.superColliderParser . Sc.alexScanTokens

rw = Sc.scExpressionPrint . scExpressionRewriteKeyword . Sc.superColliderParser . Sc.alexScanTokens

-}
