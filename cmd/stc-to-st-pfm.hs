import Sound.SC3.UGen.DB.Bindings.Smalltalk {- hsc3-db -}

import Language.Smalltalk.Ansi.Expr {- stsc3 -}
import Language.Smalltalk.Ansi.Expr.Print {- stsc3 -}
import Language.Smalltalk.SuperCollider.Translate {- stsc3 -}

main :: IO ()
main = do
  let tbl = scApplySelectorTable
      rw = exprPrintSt . exprRewriteImplicit tbl . stcToExpr
  interact rw
