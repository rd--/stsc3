-- | Printers (.st, .stc, .js and s-expression) for the Ansi.Expr Ast type.
module Language.Smalltalk.Ansi.Expr.Print where

import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.List as List {- hmt-base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Ansi.Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print as St {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Print.Stc as St {- stsc3 -}

-- * .stc

{- | Check that selector has correct arity, and split into parts.

>>> zipWith3 stcSelectorParts (words "* + pi apply: at:put:") [True, True, False, False, False] [1, 1, 0, 1, 2]
[["*"],["+"],["pi"],["apply"],["at","put"]]
-}
stcSelectorParts :: String -> Bool -> Int -> [String]
stcSelectorParts sel isBinOp arity =
  let parts = if isBinOp then [sel] else Split.splitOn ":" sel
      degree = if isBinOp then 1 else length parts - 1
  in if arity /= degree
      then error (show ("stcSelectorParts: illegal arity", sel, arity))
      else if isBinOp || arity == 0 then parts else init parts

{- | The apply message is elided in .stc, it's singular array argument is unpacked.
     It is an option here so the .stc printer can also print .sc code.
-}
messagePrintStc :: Bool -> Message -> String
messagePrintStc elideApply (Message sel arg) =
  let isBinOp = St.isBinarySelector sel
      selId = St.selectorIdentifier sel
      selParts = stcSelectorParts selId isBinOp (length arg)
      selStc = case uncons selParts of
        Nothing -> error "messagePrintStc"
        Just (h, t) -> intercalate ":" (h : takeWhile (/= "value") t)
  in case (selStc, arg) of
      (_, []) -> printf ".%s" selStc
      ("apply", [Array p]) ->
        if elideApply
          then printf "(%s)" (intercalate ", " (map (exprPrintStc elideApply) p))
          else printf ".apply([%s])" (intercalate ", " (map (exprPrintStc elideApply) p))
      (_, [arg1]) ->
        let p1 = exprPrintStc elideApply arg1
        in if isBinOp then printf " %s %s" selId p1 else printf ".%s(%s)" selStc p1
      _ ->
        printf ".%s(%s)" selStc (intercalate ", " (map (exprPrintStc elideApply) arg))

primitive_pp :: LambdaDefinition -> String
primitive_pp = maybe "" ((++ " ") . St.primitive_pp) . lambdaDefinitionPrimitive

{- | Parenthesise all binary operator sends.
     A more elaborate rule could be written if required.
-}
exprPrintStc :: Bool -> Expr -> String
exprPrintStc elideApply expr =
  case expr of
    Identifier i -> i
    Literal l -> St.sc_literal_pp l
    Assignment i e -> printf "%s = %s" i (exprPrintStc elideApply e)
    Send e m ->
      let template = if exprIsBinaryMessageSend expr then "(%s%s)" else "%s%s"
      in printf template (exprPrintStc elideApply e) (messagePrintStc elideApply m)
    Lambda ld arg tmp (e, r) ->
      let r' = maybe [] (return . printf "^%s" . exprPrintStc elideApply) r
          x = primitive_pp ld ++ intercalate "; " (map (exprPrintStc elideApply) e ++ r')
      in case (arg, tmp) of
          ([], []) -> printf "{ %s }" x
          (_, []) -> printf "{ arg %s; %s }" (intercalate ", " arg) x
          _ -> printf "{ arg %s; var %s; %s }" (intercalate ", " arg) (intercalate ", " tmp) x
    Array e -> printf "[%s]" (intercalate ", " (map (exprPrintStc elideApply) e))
    Init c tmp e ->
      let x = intercalate "; " (map (exprPrintStc elideApply) e)
          r = case (tmp, e) of
            ([], _) -> x
            _ -> printf "var %s; %s" (intercalate ", " tmp) x
      in maybe "" (St.sc_comment_pp False) c ++ r

-- * St

messagePrintSt :: Message -> String
messagePrintSt (Message sel arg) =
  let selId = St.selectorIdentifier sel
  in if null arg
      then " " ++ selId
      else
        let isBinOp = St.isBinarySelector sel
            selParts = map (\x -> x ++ ":") (stcSelectorParts selId isBinOp (length arg))
            interleave p = concat . zipWith (\i j -> [i, j]) p
        in if isBinOp
            then printf " %s %s" selId (exprPrintSt (arg !! 0))
            else " " ++ unwords (interleave selParts (map exprPrintSt arg))

{- | In St precedence is unary then binary then keyword, in Stc it is unary & keyword then binary.
     Parenthesise all keyword and binary sends.
     A more elaborate rule could be written if required.
-}
exprPrintSt :: Expr -> String
exprPrintSt expr =
  case expr of
    Identifier i -> i
    Literal l -> St.literal_pp l
    Assignment i e -> printf "%s := %s" i (exprPrintSt e)
    Send e m ->
      let template = if exprIsUnaryMessageSend expr then "%s%s" else "(%s%s)"
      in printf template (exprPrintSt e) (messagePrintSt m)
    Lambda _ arg tmp (e, r) ->
      let r' = maybe [] (return . ('^' :) . exprPrintSt) r
          x = intercalate ". " (map (exprPrintSt) e ++ r')
      in case (map (':' :) arg, tmp) of
          ([], []) -> printf "[ %s ]" x
          (arg', []) -> printf "[ %s | %s ]" (unwords arg') x
          (arg', _) -> printf "[ %s | | %s | %s ]" (unwords arg') (unwords tmp) x
    Array e -> printf "{ %s }" (intercalate ". " (map (exprPrintSt) e))
    Init c tmp e ->
      let x = intercalate ". " (map (exprPrintSt) e)
          r = case (tmp, e) of
            ([], _) -> x
            _ -> printf "| %s | %s" (unwords tmp) x
      in maybe "" St.comment_pp c ++ r

-- * S-Expression

-- | Tidy printer for Message, avoids trailing whitespace.
messagePrintLisp :: Message -> String
messagePrintLisp (Message s e) =
  case e of
    [] -> printf "(~ %s)" (St.selectorIdentifier s)
    _ -> printf "(~ %s %s)" (St.selectorIdentifier s) (unwords (map exprPrintLisp e))

commentPrintLisp :: St.Comment -> String
commentPrintLisp = unlines . map ("; " ++) . lines

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
exprPrintLisp :: Expr -> String
exprPrintLisp expr =
  case expr of
    Identifier i -> i
    Literal l -> St.literal_pp l
    Assignment i e -> printf "(:= %s %s)" i (exprPrintLisp e)
    Send e m -> printf "(. %s %s)" (exprPrintLisp e) (messagePrintLisp m)
    Lambda _ arg tmp (e, r) ->
      let r' = maybe [] (return . printf "(^ %s)" . exprPrintLisp) r
          x = unwords (map exprPrintLisp e ++ r')
      in case (arg, tmp) of
          ([], []) -> printf "(\\ %s)" x
          (_, []) -> printf "(\\ (: %s) %s)" (unwords arg) x
          _ -> printf "(\\ (: %s) (| %s) %s)" (unwords arg) (unwords tmp) x
    Array e -> printf "(%% %s)" (unwords (map exprPrintLisp e))
    Init c tmp e ->
      let x = unwords (map exprPrintLisp e)
          r = case (tmp, e) of
            ([], [e0]) -> exprPrintLisp e0
            ([], _) -> printf "(>> %s)" x
            _ -> printf "(>> (| %s) %s)" (unwords tmp) x
      in maybe "" commentPrintLisp c ++ r

-- * Js

{- | Js operators are not extensible, therefore .stc operators, which are, must be re-written as functions.
Logical operators however cannot be written as functions.
This table gives names to standard operators, however it is not generic, c.f. jsCharRenamingTable.
-}
jsDefaultRenamingTable :: [(String, String)]
jsDefaultRenamingTable =
  [ ("+", "Add")
  , ("-", "Sub")
  , ("*", "Mul")
  , ("/", "Fdiv")
  , ("%", "Mod")
  , ("**", "Pow")
  , (">", "Gt")
  , ("<", "Lt")
  , (">=", "Ge")
  , ("<=", "Le")
  , ("=", "Eq")
  , ("~=", "Neq")
  , ("&", "BitAnd")
  , ("|", "BitOr")
  , ("<<", "ShiftLeft")
  , (">>", "ShiftLeft")
  , ("++", "append")
  ]

-- | Table giving generic names to operator characters (.sl names).
jsCharNameTable :: [(Char, String)]
jsCharNameTable =
  [ ('!', "bang") -- exclamationMark exclamationPoint
  , ('"', "doubleQuote") -- quotationMark
  , ('#', "hash") -- numberSign
  , ('$', "dollar") -- dollarSign
  , ('%', "modulo") -- percent
  , ('&', "and") -- ampersand
  , ('*', "times") -- asterisk
  , ('+', "plus")
  , (',', "comma")
  , ('-', "minus") -- hyphen
  , ('.', "dot") -- period fullstop
  , ('/', "dividedBy") -- slash
  , (':', "colon")
  , (';', "semicolon")
  , ('<', "lessThan")
  , ('=', "equals")
  , ('>', "greaterThan")
  , ('?', "query") -- questionMark
  , ('@', "commercialAt") -- atSign
  , ('\'', "singleQuote") -- apostrophe
  , ('\\', "backslash")
  , ('^', "raisedTo") -- caret circumflex hat
  , ('_', "underscore")
  , ('`', "backtick")
  , ('|', "or") -- verticalBar
  , ('~', "tilde")
  ]

{- | A generic renamer.

> mapMaybe (jsOperatorGenericRename . fst) jsDefaultRenamingTable
-}
jsOperatorGenericRename :: String -> Maybe String
jsOperatorGenericRename operator =
  let f aChar = lookup aChar jsCharNameTable
      answer = map f operator
  in if any isNothing answer
      then Nothing
      else Just (concatMap fromJust answer)

-- | Generic renaming of the entries in the jsDefaultRenamingTable.
jsCharRenamingTable :: [(String, String)]
jsCharRenamingTable =
  let lhs = map fst jsDefaultRenamingTable
      f aString = fromMaybe aString (jsOperatorGenericRename aString)
  in zip lhs (map f lhs)

jsRenamerFromTable :: Maybe String -> [(String, String)] -> String -> String
jsRenamerFromTable maybePrefix tbl nm = fromMaybe "" maybePrefix ++ fromMaybe nm (lookup nm tbl)

{- | Checks that the arity agrees (which should be correct by construction)
     and that all subsequent keyword parts are "value".

"at:put:" is currently handled specially because in .stc "p[q] := r" is syntax for "p.at(q, put:r)".
There could also be a more general rewrite rule, so that "at:put:" was returned as "atPut",
or there could be a rewrite table with entries of the form ("at:put:", "put").

>>> zipWith3 stcSelectorJsForm (words "* + pi apply: value:value:") [True, True, False, False, False] [1, 1, 0, 1, 2]
["*","+","pi","apply","value"]

>>> stcSelectorJsForm "at:put:" False 2
"put"
-}
stcSelectorJsForm :: String -> Bool -> Int -> String
stcSelectorJsForm sel isBinOp arity =
  let parts = stcSelectorParts sel isBinOp arity -- performs arity check
  in if isBinOp || all (== "value") (List.tail_err parts)
      then List.head_err parts
      else
        if sel == "at:put:"
          then "put"
          else error ("stcSelectorJsForm: not binary operator and not all value: " ++ sel)

literalPrintJs :: St.Literal -> String
literalPrintJs l =
  case l of
    St.CharacterLiteral c -> printf "'%c'" c
    St.SymbolLiteral s -> printf "'%s'" s
    St.SelectorLiteral _ -> error "literalPrintJs: selector"
    St.ArrayLiteral a -> printf "[%s]" (intercalate ", " (map (either literalPrintJs id) a))
    _ -> St.literal_pp l

{- | Print Js notation of Expr.

>>> import Language.Smalltalk.Spl.Translate
>>> let rw = exprPrintJs (jsRenamerFromTable Nothing jsDefaultRenamingTable) . stcToExpr
>>> map rw (words "q.p q.p(r) q.p(r,s) p(q)")
["p(q)","p(q, r)","p(q, r, s)","p(q)"]

>>> map rw ["p + q * r", "p % q >= r"]
["Mul(Add(p, q), r)","Ge(Mod(p, q), r)"]

>>> map rw ["{}", "{ :x | x * x }"]
["function() {  return null; }","function(x) {  return Mul(x, x); }"]

>>> rw "{ :x | let y = x * x; x + y }"
"function(x) { var y; y = Mul(x, x); return Add(x, y); }"

>>> map rw ["{}.value", "{ :x | x * x }.value(3)"]
["(function() {  return null; })()","(function(x) {  return Mul(x, x); })(3)"]

>>> rw "{ :x :y | (x * x) + (y * y) }.value(3, 5)"
"(function(x, y) {  return Add(Mul(x, x), Mul(y, y)); })(3, 5)"

>>> map rw (words "1 2.3 'x' [5,6]")
["1","2.3","'x'","[5, 6]"]

>>> map rw (words "inf pi nil twoPi")
["inf","pi","null","twoPi"]

>>> rw "(* c *)\nx := 6; x.postln"
"// c\nx = 6; postln(x)"
-}
exprPrintJs :: (St.Identifier -> St.Identifier) -> Expr -> String
exprPrintJs rw expr =
  case expr of
    Identifier x ->
      case x of
        "nil" -> "null"
        _ -> x
    Literal x -> literalPrintJs x
    Assignment lhs rhs -> printf "%s = %s" lhs (exprPrintJs rw rhs)
    Send rcv (Message sel arg) ->
      case (rcv, stcSelectorJsForm (St.selectorIdentifier sel) (St.isBinarySelector sel) (length arg), arg) of
        (_, "apply", [Array p]) ->
          let rcv' = case rcv of
                Identifier msg -> Identifier (rw msg)
                _ -> rcv
          in printf "%s(%s)" (exprPrintJs rw rcv') (intercalate ", " (map (exprPrintJs rw) p))
        (_, "value", _) -> printf "(%s)(%s)" (exprPrintJs rw rcv) (intercalate ", " (map (exprPrintJs rw) arg))
        (Identifier "Float", "pi", []) -> (rw "pi")
        (Identifier "Float", "infinity", _) -> (rw "inf")
        (_, msg, _) -> printf "%s(%s)" (rw msg) (intercalate ", " (map (exprPrintJs rw) (rcv : arg)))
    Lambda _ arg tmp (stm, Nothing) ->
      let numStm = length stm
          (stm', ret) = if null stm then ([], Nothing) else (take (numStm - 1) stm, Just (last stm))
          ret' = maybe [] (return . printf "return %s;" . exprPrintJs rw) ret
      in printf
          "function(%s) { %s %s }"
          (intercalate ", " arg)
          (if null tmp then "" else printf "var %s;" (intercalate ", " tmp))
          (if null stm then "return null;" else intercalate "; " (map (exprPrintJs rw) stm' ++ ret'))
    Lambda {} -> error "exprPrintJs: nonLocalReturn"
    Array e -> printf "[%s]" (intercalate ", " (map (exprPrintJs rw) e))
    Init c tmp e ->
      let x = intercalate "; " (map (exprPrintJs rw) e)
          r = case (tmp, e) of
            ([], _) -> x
            _ -> printf "var %s; %s" (intercalate ", " tmp) x
      in maybe "" ("// " ++) c ++ r

-- * Scheme

literalPrintScheme :: St.Literal -> String
literalPrintScheme l =
  case l of
    St.CharacterLiteral c -> printf "#\\%c" c
    St.StringLiteral s -> printf "\"%s\"" s
    St.SymbolLiteral s -> printf "'%s" s
    St.SelectorLiteral _ -> error "literalPrintScheme: selector"
    St.ArrayLiteral a -> printf "'(%s)" (unwords (map (either literalPrintScheme id) a))
    _ -> St.literal_pp l

exprTmpStmScheme :: (St.Identifier -> St.Identifier) -> [St.Identifier] -> ([Expr], Maybe Expr) -> String
exprTmpStmScheme rw tmp (stm, ret) =
  let stm' = stm ++ maybe [] return ret
  in printf
      "(let (%s) %s))"
      (if null tmp then "" else unwords (map (\nm -> printf "(%s 'undefined)" nm) tmp))
      (if null stm' then "'()" else unwords (map (exprPrintScheme rw) stm'))

{- | Print scheme (lisp) notation of Expr.  Use the Js renaming tables.

>>> import Language.Smalltalk.Spl.Translate
>>> let rw = exprPrintScheme (jsRenamerFromTable Nothing jsDefaultRenamingTable) . stcToExpr
>>> map rw (words "q.p q.p(r) q.p(r,s) p(q)")
["(p q)","(p q r)","(p q r s)","(p q)"]

>>> map rw ["p + q * r", "p % q >= r"]
["(Mul (Add p q) r)","(Ge (Mod p q) r)"]

>>> map rw ["{}", "{ :x | x * x }"]
["(lambda () (let () '()))","(lambda (x) (let () (Mul x x)))"]

>>> rw "{ :x | let y = x * x; x + y }"
"(lambda (x) (let ((y 'undefined)) (set! y (Mul x x)) (Add x y)))"

>>> map rw ["{}.value", "{ :x | x * x }.value(3)"]
["((lambda () (let () '())) )","((lambda (x) (let () (Mul x x))) 3)"]

>>> rw "{ :x :y | let z = (x * x) + (y * y); z }.value(3, 5)"
"((lambda (x y) (let ((z 'undefined)) (set! z (Add (Mul x x) (Mul y y))) z)) 3 5)"

>>> map rw (words "1 2.3 'x' [5,6]")
["1","2.3","\"x\"","(list 5 6)"]

>>> map rw (words "inf pi nil twoPi")
["inf","pi","'()","twoPi"]

>>> rw "(* c *)\nlet x = 6; x.postln"
"; c\n(let ((x 'undefined)) (set! x 6) (postln x)))"
-}
exprPrintScheme :: (St.Identifier -> St.Identifier) -> Expr -> String
exprPrintScheme rw expr =
  case expr of
    Identifier x ->
      case x of
        "nil" -> "'()"
        _ -> x
    Literal x -> literalPrintScheme x
    Assignment lhs rhs -> printf "(set! %s %s)" lhs (exprPrintScheme rw rhs)
    Send rcv (Message sel arg) ->
      case (rcv, rw (stcSelectorJsForm (St.selectorIdentifier sel) (St.isBinarySelector sel) (length arg)), arg) of
        (_, "apply", [Array p]) -> printf "(%s %s)" (exprPrintScheme rw rcv) (unwords (map (exprPrintScheme rw) p))
        (_, "value", _) -> printf "(%s %s)" (exprPrintScheme rw rcv) (unwords (map (exprPrintScheme rw) arg))
        (Identifier "Float", "pi", []) -> "pi"
        (Identifier "Float", "infinity", _) -> "inf"
        (_, msg, _) -> printf "(%s %s)" msg (unwords (map (exprPrintScheme rw) (rcv : arg)))
    Lambda _ arg tmp stm -> printf "(lambda (%s) %s" (unwords arg) (exprTmpStmScheme rw tmp stm)
    Array e -> printf "(list %s)" (unwords (map (exprPrintScheme rw) e))
    Init c tmp stm ->
      concat
        [ maybe "" (unlines . map ("; " ++) . lines) c
        , if length stm == 1
            then exprPrintScheme rw (List.head_err stm)
            else exprTmpStmScheme rw tmp (stm, Nothing)
        ]
