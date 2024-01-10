import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Language.JavaScript.Parser as Js {- language-javascript -}
import qualified Language.JavaScript.Parser.AST as Js {- language-javascript -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import Language.Smalltalk.Ansi.Expr {- stsc3 -}
import Language.Smalltalk.Ansi.Expr.Print {- stsc3 -}

type JsAst = Js.JSAST
type JsExp = Js.JSExpression
type JsStm = Js.JSStatement

jsAstToExpr :: JsAst -> Expr
jsAstToExpr js =
  case js of
    Js.JSAstProgram stm _ -> Init Nothing [] (map jsStmToExpr stm)
    _ -> error "jsAstToExpr"

nilValue :: Expr
nilValue = Identifier "nil"

{- | Keyword selector

>>> jsGenKeywordSelector "put" 2
KeywordSelector "put:value:" 1
-}
jsGenKeywordSelector :: String -> Int -> St.Selector
jsGenKeywordSelector msg arity = St.KeywordSelector (msg ++ ":" ++ concat (replicate (arity - 1) "value:")) (arity - 1)

jsStmToExpr :: JsStm -> Expr
jsStmToExpr stm =
  case stm of
    Js.JSExpressionStatement e _ -> jsExpToExpr e
    Js.JSMethodCall (Js.JSIdentifier _ rcv) _ arg _ _ -> implicitSend rcv (jsListExp arg)
    Js.JSMethodCall (Js.JSMemberDot rcv _ (Js.JSIdentifier _ msg)) _ arg _ _ ->
      let argList = jsCommaListElem arg
      in Send
         (jsExpToExpr rcv)
         (Message (jsGenKeywordSelector msg (length argList)) (map jsExpToExpr argList))
    Js.JSVariable _ _ _ -> error "jsStmToExpr: var?"
    Js.JSReturn _ _ _ -> error "jsStmToExpr: return?"
    Js.JSAssignStatement (Js.JSIdentifier _ varName) (Js.JSAssign _) varInit _ ->
      Assignment varName (jsExpToExpr varInit)
    _ -> error ("jsStmToExpr: " ++ show stm)

jsStmIsVar :: JsStm -> Bool
jsStmIsVar stm =
  case stm of
    Js.JSVariable _ _ _ -> True
    _ -> False

type VarAssign t = (St.Identifier, Expr)

-- | ([VariableDeclaration], [Statement], Maybe ReturnExpression)
jsStmSplitVar :: [JsStm] -> ([VarAssign t], [JsStm], Maybe JsExp)
jsStmSplitVar stm =
  let (var, postVar) = span jsStmIsVar stm
  in if any jsStmIsVar postVar
     then error "jsStmSplitVar: var in postVar"
     else case unsnoc postVar of
            Just (inner, Js.JSReturn _ (Just e) _) -> (concatMap jsStmGetVar var, inner, Just e)
            _ -> (concatMap jsStmGetVar var, postVar, Nothing)

jsStmGetVar :: JsStm -> [VarAssign t]
jsStmGetVar stm =
  case stm of
    Js.JSVariable _ var _ -> map jsExprVar (jsCommaListElem var)
    _ -> error "jsStmGetVar"

jsListExp :: Js.JSCommaList JsExp -> [Expr]
jsListExp = map jsExpToExpr . jsCommaListElem

maybeVarAssign :: VarAssign t -> Maybe (Expr)
maybeVarAssign (varName, varInit) = if varInit == nilValue then Nothing else Just (Assignment varName varInit)

mkSendUnary :: JsExp -> St.Identifier -> Expr
mkSendUnary r m = Send (jsExpToExpr r) (Message (St.UnarySelector m) [])

mkSendBinary :: JsExp -> St.BinaryIdentifier -> JsExp -> Expr
mkSendBinary r m x = Send (jsExpToExpr r) (Message (St.BinarySelector m) [jsExpToExpr x])

mkSendKeyword :: JsExp -> St.Identifier -> Int -> [JsExp] -> Expr
mkSendKeyword r m k x = Send (jsExpToExpr r) (Message (St.KeywordSelector m k) [Array (map jsExpToExpr x)])

mkApply :: JsExp -> [JsExp] -> Expr
mkApply r x = mkSendKeyword r "apply:" 1 x

mkNegated :: JsExp -> Expr
mkNegated r = mkSendUnary r "negated"

jsExpToExpr :: JsExp -> Expr
jsExpToExpr js =
  case js of
    Js.JSIdentifier _ str -> Identifier str
    Js.JSLiteral _ str -> Identifier str -- i.e. true
    Js.JSDecimal _ str -> Literal (St.NumberLiteral (St.Float (read str)))
    Js.JSStringLiteral _ str -> Literal (St.StringLiteral (jsUnquote str))
    Js.JSArrayLiteral _ elm _ -> Array (mapMaybe jsArrayElementExpr elm)
    Js.JSExpressionBinary lhs op rhs -> mkSendBinary lhs (jsBinOpString op) rhs
    Js.JSExpressionParen _ e _ -> jsExpToExpr e -- ?
    Js.JSMemberDot lhs _ (Js.JSIdentifier _ rhs) ->
      Send (jsExpToExpr lhs) (Message (St.UnarySelector rhs) [])
    Js.JSMemberExpression f _ x _ -> mkApply f (jsCommaListElem x)
    Js.JSVarInitExpression (Js.JSIdentifier _ _varName) _varInit -> error "jsExpToExpr: JSVarInitExpression?"
    Js.JSArrowExpression arg _ stm ->
      Lambda NullLambda (jsArrowParameterListIdent arg) [] ([jsStmToExpr stm], Nothing)
    Js.JSFunctionExpression _ Js.JSIdentNone _ arg _ (Js.JSBlock _ stm _) ->
      let (var, nonVar, ret) = jsStmSplitVar stm
      in Lambda
         NullLambda
         (map jsExprIdentifier (jsCommaListElem arg))
         (map fst var)
         ((mapMaybe maybeVarAssign var ++ map jsStmToExpr nonVar), fmap jsExpToExpr ret)
    Js.JSMemberSquare lhs _ rhs _ ->
      Send (jsExpToExpr lhs) (Message (St.KeywordSelector "at:" 1) [jsExpToExpr rhs])
    Js.JSUnaryExpression (Js.JSUnaryOpMinus _) e -> mkNegated e
    _ -> error ("jsExpToExpr: " ++ show js)

jsExprVar :: JsExp -> VarAssign t
jsExprVar js =
  case js of
    Js.JSVarInitExpression (Js.JSIdentifier _ varName) varInit ->
      (varName, jsVarInitializerExpr varInit)
    _ -> error "jsExprVar"

jsArrayElementExpr :: Js.JSArrayElement -> Maybe Expr
jsArrayElementExpr e =
  case e of
    Js.JSArrayElement x -> Just (jsExpToExpr x)
    Js.JSArrayComma _ -> Nothing

jsUnquote :: String -> String
jsUnquote s =
  case s of
    _:x -> init x
    _ -> error "jsUnquote"

jsExprIdentifier :: JsExp -> String
jsExprIdentifier js =
  case js of
    Js.JSIdentifier _ str -> str
    _ -> error "jsExprIdentifier"

jsArrowParameterListIdent :: Js.JSArrowParameterList -> [St.Identifier]
jsArrowParameterListIdent p =
  case p of
    Js.JSUnparenthesizedArrowParameter (Js.JSIdentName _ x) -> [x]
    Js.JSParenthesizedArrowParameterList _ l _ -> map jsExprIdentifier (jsCommaListElem l)
    _ -> error "jsArrowParameterListIdent"

jsVarInitializerExpr :: Js.JSVarInitializer -> Expr
jsVarInitializerExpr v =
  case v of
    Js.JSVarInit _ e -> jsExpToExpr e
    Js.JSVarInitNone -> nilValue -- ?

jsCommaListElem :: Js.JSCommaList a -> [a]
jsCommaListElem l =
  case l of
    Js.JSLCons lhs _ rhs -> jsCommaListElem lhs ++ [rhs]
    Js.JSLOne e -> [e]
    Js.JSLNil -> []

jsBinOpString :: Js.JSBinOp -> String
jsBinOpString op =
  case op of
    Js.JSBinOpAnd        _ -> "&&"
    Js.JSBinOpBitAnd     _ -> "&"
    Js.JSBinOpBitOr      _ -> "|"
    Js.JSBinOpBitXor     _ -> "^"
    Js.JSBinOpDivide     _ -> "/"
    Js.JSBinOpEq         _ -> "=="
    Js.JSBinOpGe         _ -> ">="
    Js.JSBinOpGt         _ -> ">"
    Js.JSBinOpIn         _ -> "in"
    Js.JSBinOpInstanceOf _ -> "instanceof"
    Js.JSBinOpLe         _ -> "<="
    Js.JSBinOpLsh        _ -> "<<"
    Js.JSBinOpLt         _ -> "<"
    Js.JSBinOpMinus      _ -> "-"
    Js.JSBinOpMod        _ -> "%"
    Js.JSBinOpNeq        _ -> "!="
    Js.JSBinOpOf         _ -> "of"
    Js.JSBinOpOr         _ -> "||"
    Js.JSBinOpPlus       _ -> "+"
    Js.JSBinOpRsh        _ -> ">>"
    Js.JSBinOpStrictEq   _ -> "==="
    Js.JSBinOpStrictNeq  _ -> "!=="
    Js.JSBinOpTimes      _ -> "*"
    Js.JSBinOpUrsh       _ -> ">>>"

{- | Js to Stc

>>> jsToStc "1 + 2"
"(1.0 + 2.0)"

>>> jsToStc "a + b"
"(a + b)"

>>> jsToStc "p.q"
"p.q"

>>> jsToStc "p.q.r"
"p.q.r"

>>> "(a + b).m"
"(a + b).m"

>>> "p.q(r)"
"p.q(r)"

>>> jsToStc "p.q.r(s)"
"p.q.r(s)"

>>> jsToStc "p.q(r, s)"
"p.q(r, s)"

>>> jsToStc "f(x)"
"f(x)"

>>> jsToStc "x => x"
"{ arg x; x }"

>>> jsToStc "(x, y) => (x * x) + (y * y)"
"{ arg x, y; ((x * x) + (y * y)) }"

>>> jsToStc "function(x) { x * x }"
"{ arg x; (x * x) }"

>>> jsToStc "function(x) { return x * x; }"
"{ arg x; ^(x * x) }"

>>> jsToStc "function(x) { var y; y = x * x; return y; }"
"{ arg x; var y; y = (x * x); ^y }"

>>> jsToStc "function(x) { var y = x * x; return y; }"
"{ arg x; var y; y = (x * x); ^y }"

>>> jsToStc "function(x) { var y = x * x; var z = y * y; return z; }"
"{ arg x; var y, z; y = (x * x); z = (y * y); ^z }"

>>> jsToStc "function(x) { var y = x * x, z = y * y; return z; }"
"{ arg x; var y, z; y = (x * x); z = (y * y); ^z }"

>>> jsToStc "function (x, y) { var p = x * x; var q = y * y; return p + q; }"
"{ arg x, y; var p, q; p = (x * x); q = (y * y); ^(p + q) }"

>>> jsToStc "'x'"
"\"x\""

>>> jsToStc "x[y]"
"x.at(y)"

>>> jsToStc "[1, 2, 3]"
"[1.0, 2.0, 3.0]"

>>> jsToStc "[1, 2, 3, 4, 5].find(x => x > 3)"
"[1.0, 2.0, 3.0, 4.0, 5.0].find({ arg x; (x > 3.0) })"

>>> jsToStc "true"
"true"

> jsToStc "var x = 5; x"
"var x = 5.0; x"

>>> jsToStc "f(g(h(x)))"
"f(g(h(x)))"

>>> jsToStc "MulAdd(SinOsc(((MulAdd(Rand(36, 72), 1, 0)).midiCps), 0), 0.1, 0)"
"MulAdd(SinOsc(MulAdd(Rand(36.0, 72.0), 1.0, 0.0).midiCps, 0.0), 0.1, 0.0)"

>>> jsToStc "-3"
"3.0.negated"

>>> jsToStc "Math.abs(-3)"
"Math.abs(3.0.negated)"

-}
jsToStc :: String -> String
jsToStc = exprPrintStc True . jsAstToExpr . Js.readJs

main :: IO ()
main = interact jsToStc

{-

```$ doctest 2021-11-19.hs
Examples: 28  Tried: 28  Errors: 0  Failures: 0
$
```

-}
