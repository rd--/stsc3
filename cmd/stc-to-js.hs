import qualified Language.Smalltalk.Ansi.Expr.Print as St {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

main :: IO ()
main = interact (St.exprPrintJs (St.jsRenamerFromTable St.jsDefaultRenamingTable) . Sc.stcToExpr)
