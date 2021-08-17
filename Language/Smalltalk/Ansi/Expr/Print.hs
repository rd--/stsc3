-- | An s-expression printer for the Ansi.Expr Ast type.
module Language.Smalltalk.Ansi.Expr.Print where

import Text.Printf {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Ansi.Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}

-- | Tidy printer for Message, avoids trailing whitespace.
messagePrint :: Message t -> String
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
     The arguments operator is ':'.
     The temporaries operator is '|'.
     The array operator is '%'.
     The sequence operator is '>>'.
-}
exprPrint :: Expr t -> String
exprPrint expr =
  case expr of
    Identifier i -> i
    Literal l -> St.literal_pp l
    Assignment i e -> printf "(:= %s %s)" i (exprPrint e)
    Return e -> printf "(^ %s)" (exprPrint e)
    Send e m -> printf "(. %s %s)" (exprPrint e) (messagePrint m)
    Lambda _ a (St.Temporaries t) e ->
      let x = unwords (map exprPrint e)
      in case (a,t) of
        ([],[]) -> printf "(\\ %s)" x
        (_,[]) -> printf "(\\ (: %s) %s)" (unwords a) x
        _ -> printf "(\\ (: %s) (| %s) %s)" (unwords a) (unwords t) x
    Array e -> printf "(%% %s)" (unwords (map exprPrint e))
    Begin e ->
      case e of
        [x] -> exprPrint x
        _ -> printf "(>> %s)" (unwords (map exprPrint e))
    Init (St.Temporaries t) e ->
      let x = unwords (map exprPrint e)
      in case (t,e) of
        ([],[e0]) -> exprPrint e0
        ([],_) -> printf "(>> %s)" x
        _ -> printf "(>> (| %s) %s)" (unwords t) x
