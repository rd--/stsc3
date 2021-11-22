-- | Rewriter for a simple class of UGen graphs as Ndef graphs.
module Language.Smalltalk.SuperCollider.Ndef where

import Data.Char {- base -}
import Data.List {- base -}

import qualified Language.Smalltalk.Ansi as St {- stsc3 -}
import           Language.Smalltalk.Ansi.Expr {- stsc3 -}
import qualified Language.Smalltalk.Ansi.Expr.Print as St {- stsc3 -}

import           Language.Smalltalk.SuperCollider.Ast {- stsc3 -}
import qualified Language.Smalltalk.SuperCollider.Translate as Sc {- stsc3 -}

-- | x -> Ndef('x')
ndef_ref :: St.Symbol -> Expr t
ndef_ref x = implicitSend "Ndef" [symbolLiteral x]

-- | x -> Ndef('x').perform(Ndef('x').bus.rate.rateToSelector)
ndef_ref_rt :: St.Symbol -> Expr t
ndef_ref_rt x =
  keywordSend
  (ndef_ref x)
  "perform:"
  [unarySendSequence (ndef_ref x) ["bus", "rate", "rateToSelector"]]

-- | A name that won't be typed by a person
uniq_sym :: String
uniq_sym = "z__"

-- | Assign last statement to uniq_sym and add play instruction.
with_assign_and_play :: [Expr t] -> [Expr t]
with_assign_and_play l =
  case splitAt (length l - 1) l of
    (p,[q]) -> p ++ [Assignment uniq_sym q
                    ,unarySend (ndef_ref uniq_sym) "play"]
    _ -> error "with_assign_and_play?"

-- | x = y -> Ndef('x', { y }) & x -> Ndef('x').
to_ndef ::  Expr t -> Expr t
to_ndef expr =
  case expr of
    Identifier i ->
      if isLower (head i) && i `notElem` scPseudoVariables then ndef_ref_rt i else Identifier i
    Assignment p q ->
      implicitSend "Ndef" [symbolLiteral p, inLambda q]
    _ -> expr

-- | Rewrite simple .stc Ugen graph as Ndef graph.
stcUgenToNdef :: String -> String
stcUgenToNdef =
  (++ "\n") .
  intercalate ";\n" .
  map St.exprPrintStc .
  map (expr_map to_ndef) .
  with_assign_and_play .
  initStatements .
  initializerDefinitionExpr .
  Sc.stcParseInitializerDefinition
