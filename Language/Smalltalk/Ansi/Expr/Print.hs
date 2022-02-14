-- | Printers (.stc and s-expression) for the Ansi.Expr Ast type.
module Language.Smalltalk.Ansi.Expr.Print where

import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Data.List.Split as Split {- split -}

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
    Init c (St.Temporaries t) e ->
      let x = intercalate "; "  (map exprPrintStc e)
          r = case (t,e) of
                ([],_) ->  x
                _ -> printf "var %s; %s" (intercalate ", " t) x
      in maybe "" St.sc_comment_pp c ++ r

-- * S-Expression

-- | Tidy printer for Message, avoids trailing whitespace.
messagePrintLisp :: Message t -> String
messagePrintLisp (Message s e) =
  case e of
    [] -> printf "(~ %s)" (St.selectorIdentifier s)
    _ -> printf "(~ %s %s)" (St.selectorIdentifier s) (unwords (map exprPrintLisp e))

commentPrintLisp :: St.Comment -> String
commentPrintLisp = unlines . map ("; " ++ ) . lines

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
    Init c (St.Temporaries t) e ->
      let x = unwords (map exprPrintLisp e)
          r = case (t,e) of
            ([],[e0]) -> exprPrintLisp e0
            ([],_) -> printf "(>> %s)" x
            _ -> printf "(>> (| %s) %s)" (unwords t) x
      in maybe "" commentPrintLisp c ++ r

-- * Js

{- | Js operators are not extensible, therefore .stc operators, which are, must be re-written as functions.
     Logical operators however cannot be written as functions.
-}
jsDefaultRenamingTable :: [(String, String)]
jsDefaultRenamingTable =
  [("+", "add"), ("-", "sub"), ("*", "mul"), ("/", "fdiv"), ("%", "mod"), ("**", "pow")
  ,(">", "gt"), ("<", "lt"), (">=", "ge"), ("<=", "le"), ("==", "eq"), ("!=", "neq")
  ,("&", "bitAnd"), ("|", "bitOr"), ("<<", "shiftLeft"), (">>", "shiftLeft")
  ,("++", "append")]

jsRenamerFromTable :: [(String, String)] -> String -> String
jsRenamerFromTable tbl nm = fromMaybe nm (lookup nm tbl)

literalPrintJs :: St.Literal -> String
literalPrintJs l =
  case l of
    St.CharacterLiteral c -> printf "'%c'" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral _ -> error "literalPrintJs: selector"
    St.ArrayLiteral a -> printf "[%s]" (intercalate ", " (map (either St.literal_pp id) a))
    _ -> St.literal_pp l


{- | Checks that all the arity agrees (which should be correct by construction)
     and that all subsequeny keyword parts are "value".

> zipWith stcSelectorJsForm (words "* + pi apply: value:value:") [1, 1, 0, 1, 2]
-}
stcSelectorJsForm :: String -> Bool -> Int -> String
stcSelectorJsForm sel isBinOp arity =
  let parts = if isBinOp then [sel] else Split.splitOn ":" sel
      degree = if isBinOp then 1 else length parts - 1
      msg = head parts
  in if arity /= degree
     then error (show ("stcSelectorJsForm: illegal arity", sel, arity))
     else if degree == 0
          then msg
          else if isBinOp || (all (== "value") (init (tail parts)) && last parts == "")
               then msg
               else error ("stcSelectorJsForm: " ++ sel)

{- | Print Js notation of Expr.

import Language.Smalltalk.SuperCollider.Translate {- stsc3 -}
rw = exprPrintJs (jsRenamerFromTable jsDefaultRenamingTable) . stcToExpr
map rw (words "q.p q.p(r) q.p(r,s) p(q)")
map rw ["p + q * r", "p % q >= r"]
map rw ["{}", "{ arg x; x * x }", "{ arg x; var y = x * x; x + y }"]
map rw ["{}.value", "{ arg x; x * x }.value(3)", "{ arg x, y; (x * x) + (y * y) }.value(3, 5)"]
map rw (words "1 2.3 \"4\" $c 'x' #[5,6]")
map rw (words "inf pi nil twoPi")
rw "// c\nx = 6; x.postln"
rw "p:q:r(1, 2, 3)" -- interior colons not allowed
-}
exprPrintJs :: (St.Identifier -> St.Identifier) -> Expr t -> String
exprPrintJs rw expr =
  case expr of
    Identifier x ->
      case x of
        "nil" -> "null"
        _ -> x
    Literal x -> literalPrintJs x
    Assignment lhs rhs -> printf "%s = %s" lhs (exprPrintJs rw rhs)
    Return e -> printf "return %s;" (exprPrintJs rw e)
    Send rcv (Message sel arg) ->
      case (rcv, rw (stcSelectorJsForm (St.selectorIdentifier sel) (St.isBinarySelector sel) (length arg)), arg) of
        (_, "apply", [Array p]) -> printf "%s(%s)" (exprPrintJs rw rcv) (intercalate ", " (map (exprPrintJs rw) p))
        (_, "value", _) -> printf "(%s)(%s)" (exprPrintJs rw rcv) (intercalate ", " (map (exprPrintJs rw) arg))
        (Identifier "Float", "pi", []) -> "pi"
        (Identifier "Float", "infinity", _) -> "inf"
        (_, msg, _) -> printf "%s(%s)" msg (intercalate ", " (map (exprPrintJs rw) (rcv : arg)))
    Lambda _ arg (St.Temporaries tmp) stm ->
      printf
      "function(%s) { %s %s }"
      (intercalate ", " arg)
      (if null tmp then "" else printf "var %s;" (intercalate ", " tmp))
      (if null stm then "return null;" else (intercalate "; " (map (exprPrintJs rw) (init stm ++ [Return (last stm)]))))
    Array e -> printf "[%s]" (intercalate ", " (map (exprPrintJs rw) e))
    Begin e -> intercalate "; "  (map (exprPrintJs rw) e)
    Init c (St.Temporaries t) e ->
      let x = intercalate "; "  (map (exprPrintJs rw) e)
          r = case (t,e) of
                ([],_) ->  x
                _ -> printf "var %s; %s" (intercalate ", " t) x
      in maybe "" St.sc_comment_pp c ++ r
