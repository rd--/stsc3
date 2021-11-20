-- | Rewriter for a simple class of UGen graphs as Ndef graphs.
module Language.Smalltalk.SuperCollider.Ndef where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Ansi.Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr.Print as St {- stsc3 -}

import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

-- | Fetch statements, discard temporaries.
from_init :: Expr t -> [Expr t]
from_init expr =
  case expr of
    Init _ x -> x
    _ -> error "from_init?"

-- | e.m
unary_send :: Expr t -> St.Identifier -> Expr t
unary_send e m = Send e (Message (St.UnarySelector m) [])

-- | e.m1.m2 &etc. (left to right)
unary_seq :: Expr t -> [St.Identifier] -> Expr t
unary_seq e m =
  case m of
    [] -> e
    m1 : m' -> unary_seq (unary_send e m1) m'

sym_lit :: St.Symbol -> Expr t
sym_lit = Literal . St.SymbolLiteral

-- | e.k(l)
keyword_send :: Expr t -> St.Symbol -> [Expr t] -> Expr t
keyword_send e k l = Send e (Message (St.KeywordSelector (k ++ ":")) l)

-- | e(l) -> e.apply([l])
implicit_send :: Expr t -> [Expr t] -> Expr t
implicit_send e l = keyword_send e "apply" [Array l]

-- | x -> Ndef('x')
ndef_ref :: St.Symbol -> Expr t
ndef_ref x = implicit_send (Identifier "Ndef") [sym_lit x]

-- | x -> Ndef('x').perform(Ndef('x').bus.rate.rateToSelector)
ndef_ref_rt :: St.Symbol -> Expr t
ndef_ref_rt x =
  keyword_send (ndef_ref x) "perform" [unary_seq (ndef_ref x) ["bus", "rate", "rateToSelector"]]

-- | A name that won't be typed by a person
uniq_sym :: String
uniq_sym = "z__"

-- | Assign last statement to uniq_sym and add play instruction.
with_assign_and_play :: [Expr t] -> [Expr t]
with_assign_and_play l =
  case splitAt (length l - 1) l of
    (p,[q]) -> p ++ [Assignment uniq_sym q
                    ,unary_send (ndef_ref uniq_sym) "play"]
    _ -> error "with_assign_and_play?"

-- | x -> { x }
to_thunk :: Expr t -> Expr t
to_thunk x = Lambda NullLambda [] St.emptyTemporaries [x]

-- | x = y -> Ndef('x', { y }) & x -> Ndef('x').
to_ndef ::  Expr t -> Expr t
to_ndef expr =
  case expr of
    Identifier i ->
      if isLower (head i) && i `notElem` scPseudoVariables then ndef_ref_rt i else Identifier i
    Assignment p q ->
      implicit_send (Identifier "Ndef") [sym_lit p, to_thunk q]
    _ -> expr

-- | Rewrite simple .stc Ugen graph as Ndef graph.
stcUgenToNdef :: String -> String
stcUgenToNdef =
  (++ "\n") .
  intercalate ";\n" .
  map St.exprPrintStc .
  map (expr_map to_ndef) .
  with_assign_and_play .
  from_init .
  initializerDefinitionExpr .
  Sc.stcParseInitializerDefinition
