module Language.Smalltalk.Ansi.Expr.Print where

import Text.Printf {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Ansi.Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

messagePrint :: Message -> String
messagePrint (Message s e) =
  case e of
    [] -> printf "(~ %s)" (St.selectorIdentifier s)
    _ -> printf "(~ %s %s)" (St.selectorIdentifier s) (unwords (map exprPrint e))

{- | S-expression printer.
     The message constructor is '~'.
     The assignment (set) operator is ':='.
     The return operator is '^'.
     The send operator is '.'.
     The lambda operator is '\'.
     The array operator is '%'.
-}
exprPrint :: Expr -> String
exprPrint expr =
  case expr of
    Identifier i -> i
    Literal l -> St.literal_pp l
    Assignment i e -> printf "(:= %s %s)" i (exprPrint e)
    Return e -> printf "(^ %s)" (exprPrint e)
    Send e m -> printf "(. %s %s)" (exprPrint e) (messagePrint m)
    Lambda a (St.Temporaries t) e -> printf "(\\ (%s) (let (%s) %s))" (unwords a) (unwords t) (unwords (map exprPrint e))
    Array e -> printf "(%% %s)" (unwords (map exprPrint e))
