{- | Rewrite Sc Ast to conform to Smalltalk Ast rules.

The three main tasks are:

- changing the precedence rules
- rewriting variable declarations
- packaging method parameters into a dictionary (array of associations) (not required for .stc -> .st)

The SuperCollider message precedence rule is Dot > Binary.
The Smalltalk message precedence rule is Unary > Binary > Keyword.
A SuperCollider Dot message is either Unary or Keyword.
To translate, a Dot message sequence has parentheses inserted after each Keyword element.

- p.q(a).r          => (p q: a) r                  <= p q: a r   ==> p q: (a r)
- p.q.r(a).s(b).t   => ((p q r: a) s: b) t
- p.q(a) + r        => (p q: a) + r
- p.q(a).r + s      => (p q: a) r + s
- p + q.r(a)        => p + (q r: a)                <= p + q r: a ==> (p + q) r: a

This transformation at the Sc Ast.

- p.q(a).r          => (p.q(a)).r
- p.q.r(a).s(b).t   => (((p.q.r(a))).s(b)).t
- p.q(a) + r        => (p.q(a)) + r
- p.q(a).r + s      => (p.q(a)).r + s
- p + q.r(a)        => p + (q.r(a))

Temporary variables in SuperCollider may have initialisers and there may be multiple sets.
These must be rewritten to as a single set of names and a sequence of assignments.

- var p, q = a;     => |p q| q := a.
- var p=a; var q;   => |p q| p := a.

This transformation at the Sc Ast.

- var p, q = a;     => var p, q; q = a;
- var p = a; var q; => var p, q; p = a;

Method parameters are collated into an array of either plain values or (keyword,value) associations.

- p.q(a,b)          => p q: {a. b}
- p.q(x: a, b)      => p q: {#x: -> a. b}

This transformation at the Sc Ast.

- p.q(a,b)          => p.q([a,b])
- p.q(x: a, b)      => p.q([\x: -> a,b])
-}
module Language.Smalltalk.SuperCollider.Rewrite where

import Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Ast.Print as Sc
import qualified Language.Smalltalk.SuperCollider.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Parser as Sc {- stsc3 -}

import Language.Smalltalk.SuperCollider.Rewrite.Precedence
import Language.Smalltalk.SuperCollider.Rewrite.Temporaries

scExpressionRewrite :: ScExpression -> ScExpression
scExpressionRewrite =
  scExpressionRewritePrecedence
    . scExpressionRewriteTemporaries

scBlockBodyRewrite :: ScBlockBody -> ScBlockBody
scBlockBodyRewrite =
  scBlockBodyRewritePrecedence
    . scBlockBodyRewriteTemporaries

{- | Option to rewrite n-ary expressions as arrays.
     Else N-ary expressions will be an error on translation.
-}
scInitializerDefinitionRewrite :: ScInitializerDefinition -> ScInitializerDefinition
scInitializerDefinitionRewrite =
  scInitializerDefinitionRewritePrecedence
    . scInitializerDefinitionRewriteTemporaries

-- | Viewer for rewriter. Reads, rewrites and prints Sc expression.
scRewriteViewer :: String -> String
scRewriteViewer =
  Sc.scInitializerDefinitionPrint
    . scInitializerDefinitionRewrite
    . Sc.superColliderParserInitializerDefinition
    . Sc.alexScanTokens

{-

rw = scRewriteViewer False
rw "p.q" == "p.q\n"
rw "p.q.r" == "p.q.r\n"
rw "p.q + r" == "p.q + r\n"
rw "p.q(x)" == "p.q(x)\n"
rw "p.q(x) + r" == "(p.q(x)) + r\n"

rw = scRewriteViewer True
rw "p.q(x)" == "p.q([x])" -- only one message, can be keyword
rw "p.q(x).r(y)" == "(p.q([x])).r([y])" -- nested trailing
rw "p.q(x) + r" == "(p.q([x])) + r" -- parens ; singular requires if initial of binary, c.f. p.q(a)
rw "p.q + r" == "(p.q([])) + r" -- unary is the same

-}
