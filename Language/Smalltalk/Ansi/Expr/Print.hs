-- | Printers (.stc and s-expression) for the Ansi.Expr Ast type.
module Language.Smalltalk.Ansi.Expr.Print where

import Data.List {- base -}
import Text.Printf {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Ansi.Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print.SuperCollider as St {- stsc3 -}

-- * .stc

-- | Is Expr a binary operator message send.
stcIsBinaryMessageSend :: Expr t -> Bool
stcIsBinaryMessageSend expr =
  case expr of
    Send _lhs (Message s [_rhs]) -> St.isBinarySelector s
    _ -> False

-- | The apply message is elided in .stc, it's singular array argument is unpacked.
messagePrintStc :: Message t -> String
messagePrintStc (Message s e) =
  let i = takeWhile (/= ':') (St.selectorIdentifier s)
  in case (i,e) of
       (_,[]) -> printf ".%s" i
       ("apply",[Array p]) -> printf "(%s)" (intercalate ", " (map exprPrintStc p))
       (_,[e1]) -> let p1 = exprPrintStc e1
                   in if St.isBinarySelector s then printf " %s %s" i p1 else printf ".%s(%s)" i p1
       _ -> printf ".%s(%s)" i (intercalate ", " (map exprPrintStc e))

{- | Parenthesise all binary operator sends.
     A more elaborate rule could be written if required.
-}
exprPrintStc :: Expr t -> String
exprPrintStc expr =
  case expr of
    Identifier i -> i
    Literal l -> St.sc_literal_pp l
    Assignment i e -> printf "%s = %s" i (exprPrintStc e)
    Return e -> printf "^%s" (exprPrintStc e)
    Send e m ->
      let template = if stcIsBinaryMessageSend expr then "(%s%s)" else "%s%s"
      in printf template (exprPrintStc e) (messagePrintStc m)
    Lambda _ a (St.Temporaries t) e ->
      let x = intercalate "; " (map exprPrintStc e)
      in case (a,t) of
        ([],[]) -> printf "{ %s }" x
        (_,[]) -> printf "{ arg %s; %s }" (intercalate ", " a) x
        _ -> printf "{ arg %s; var %s; %s }" (intercalate ", " a) (intercalate ", " t) x
    Array e -> printf "[%s]" (intercalate ", " (map exprPrintStc e))
    Begin e -> intercalate "; "  (map exprPrintStc e)
    Init (St.Temporaries t) e ->
      let x = intercalate "; "  (map exprPrintStc e)
      in case (t,e) of
        ([],_) ->  x
        _ -> printf "var %s; %s" (intercalate ", " t) x

-- * S-Expression

-- | Tidy printer for Message, avoids trailing whitespace.
messagePrintLisp :: Message t -> String
messagePrintLisp (Message s e) =
  case e of
    [] -> printf "(~ %s)" (St.selectorIdentifier s)
    _ -> printf "(~ %s %s)" (St.selectorIdentifier s) (unwords (map exprPrintLisp e))

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
exprPrintLisp :: Expr t -> String
exprPrintLisp expr =
  case expr of
    Identifier i -> i
    Literal l -> St.literal_pp l
    Assignment i e -> printf "(:= %s %s)" i (exprPrintLisp e)
    Return e -> printf "(^ %s)" (exprPrintLisp e)
    Send e m -> printf "(. %s %s)" (exprPrintLisp e) (messagePrintLisp m)
    Lambda _ a (St.Temporaries t) e ->
      let x = unwords (map exprPrintLisp e)
      in case (a,t) of
        ([],[]) -> printf "(\\ %s)" x
        (_,[]) -> printf "(\\ (: %s) %s)" (unwords a) x
        _ -> printf "(\\ (: %s) (| %s) %s)" (unwords a) (unwords t) x
    Array e -> printf "(%% %s)" (unwords (map exprPrintLisp e))
    Begin e ->
      case e of
        [x] -> exprPrintLisp x
        _ -> printf "(>> %s)" (unwords (map exprPrintLisp e))
    Init (St.Temporaries t) e ->
      let x = unwords (map exprPrintLisp e)
      in case (t,e) of
        ([],[e0]) -> exprPrintLisp e0
        ([],_) -> printf "(>> %s)" x
        _ -> printf "(>> (| %s) %s)" (unwords t) x
