{- | Rewrite Sc Ast to conform to Smalltalk Ast rules.

The three main tasks are:

- changing the precedence rules
- rewriting variable declarations
- packaging method parameters into a dictionary (array of associations) (not required for .stc -> .st)

The Stc message precedence rule is Dot > Binary.
The Smalltalk message precedence rule is Unary > Binary > Keyword.
A Stc Dot message is either Unary or Keyword.
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

Temporary variables in Stc may have initialisers and there may be multiple sets.
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
module Language.Smalltalk.Stc.Rewrite where

import Language.Smalltalk.Stc.Ast {- stsc3 -}
import qualified Language.Smalltalk.Stc.Ast.Print as Sc
import qualified Language.Smalltalk.Stc.Lexer as Sc {- stsc3 -}
import qualified Language.Smalltalk.Stc.Parser as Sc {- stsc3 -}

import Language.Smalltalk.Stc.Rewrite.Precedence
import Language.Smalltalk.Stc.Rewrite.Temporaries

stcExpressionRewrite :: StcExpression -> StcExpression
stcExpressionRewrite =
  stcExpressionRewritePrecedence
    . stcExpressionRewriteTemporaries

stcBlockBodyRewrite :: StcBlockBody -> StcBlockBody
stcBlockBodyRewrite =
  stcBlockBodyRewritePrecedence
    . stcBlockBodyRewriteTemporaries

{- | Option to rewrite n-ary expressions as arrays.
     Else N-ary expressions will be an error on translation.
-}
stcInitializerDefinitionRewrite :: StcInitializerDefinition -> StcInitializerDefinition
stcInitializerDefinitionRewrite =
  stcInitializerDefinitionRewritePrecedence
    . stcInitializerDefinitionRewriteTemporaries

-- | Viewer for rewriter. Reads, rewrites and prints Sc expression.
stcRewriteViewer :: String -> String
stcRewriteViewer =
  Sc.stcInitializerDefinitionPrint
    . stcInitializerDefinitionRewrite
    . Sc.stcParserInitializerDefinition
    . Sc.alexScanTokens

{-

rw = stcRewriteViewer False
rw "p.q" == "p.q\n"
rw "p.q.r" == "p.q.r\n"
rw "p.q + r" == "p.q + r\n"
rw "p.q(x)" == "p.q(x)\n"
rw "p.q(x) + r" == "(p.q(x)) + r\n"

rw = stcRewriteViewer True
rw "p.q(x)" == "p.q([x])" -- only one message, can be keyword
rw "p.q(x).r(y)" == "(p.q([x])).r([y])" -- nested trailing
rw "p.q(x) + r" == "(p.q([x])) + r" -- parens ; singular requires if initial of binary, c.f. p.q(a)
rw "p.q + r" == "(p.q([])) + r" -- unary is the same

-}
