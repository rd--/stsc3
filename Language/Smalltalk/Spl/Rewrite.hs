{- | Rewrite Sc Ast to conform to Smalltalk Ast rules.

The three main tasks are:

- changing the precedence rules
- rewriting variable declarations
- packaging method parameters into a dictionary (array of associations) (not required for .stc -> .st)

The Spl message precedence rule is Dot > Binary.
The Smalltalk message precedence rule is Unary > Binary > Keyword.
A Spl Dot message is either Unary or Keyword.
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

Temporary variables in Spl may have initialisers and there may be multiple sets.
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
module Language.Smalltalk.Spl.Rewrite where

import Language.Smalltalk.Spl.Ast {- stsc3 -}
import qualified Language.Smalltalk.Spl.Ast.Print as Sc {- stsc3 -}
import qualified Language.Smalltalk.Spl.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.Spl.Parser as Sc {- stsc3 -}

import qualified Language.Smalltalk.Spl.Rewrite.Precedence as Precedence
import qualified Language.Smalltalk.Spl.Rewrite.Temporaries as Temporaries

scExpressionRewrite :: ScExpression -> ScExpression
scExpressionRewrite =
  Precedence.scExpressionRewritePrecedence
    . Temporaries.scExpressionRewriteTemporaries

scBlockBodyRewrite :: ScBlockBody -> ScBlockBody
scBlockBodyRewrite =
  Precedence.scBlockBodyRewritePrecedence
    . Temporaries.scBlockBodyRewriteTemporaries

{- | Option to rewrite n-ary expressions as arrays.
     Else N-ary expressions will be an error on translation.
-}
scInitializerDefinitionRewrite :: ScInitializerDefinition -> ScInitializerDefinition
scInitializerDefinitionRewrite =
  Precedence.scInitializerDefinitionRewritePrecedence
    . Temporaries.scInitializerDefinitionRewriteTemporaries

{- | Viewer for rewriter. Reads, rewrites and prints Sc expression.


>>> let rw = scRewriteViewer
>>> rw "p.q"
"p.q\n"

>>> rw "p.q.r"
"p.q.r\n"

>>> rw "p.q + r"
"p.q + r\n"

>>> rw "p.q(x)"
"p.q(x)\n"

>>> rw "p.q(x) + r"
"(p.q(x)) + r\n"

>>> rw "p.q(x).r(y)"
"(p.q(x)).r(y)\n"
-}
scRewriteViewer :: String -> String
scRewriteViewer =
  Sc.scInitializerDefinitionPrint
    . scInitializerDefinitionRewrite
    . Sc.superColliderParserInitializerDefinition
    . Sc.alexScanTokens
