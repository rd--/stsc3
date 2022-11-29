import Data.List {- base -}
import Data.Maybe {- base -}
import Text.Printf {- base -}

import Sound.Sc3.Ugen.Db as Db {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Js as Js {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Som as Som {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.SuperCollider as Sc {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Smalltalk as St {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Bindings.Spl as Spl {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Record as Record {- hsc3-db -}

{- | Unary operators

     Sc>>log is natural, ie. St>>ln
     Sc>>log2 is base 2, ie. St>>log2
     Sc>>log10 is base 10, ie. St>>log

> map snd uop
-}
uop :: [(String, Int)]
uop =
  [("negated",0),("abs",5),("ceiling",8),("floor",9)
  ,("frac",10), ("sign", 11), ("squared",12),("cubed",13),("sqrt",14),("exp",15),("reciprocal",16)
  ,("midiCps",17),("cpsMidi",18), ("midiRatio", 19), ("ratioMidi", 20)
  ,("dbAmp", 21), ("ampDb", 22), ("ln",25),("log2",26),("log",27),("sin",28),("cos",29),("tan", 30),("tanh",36)
  ,("distort",42),("softClip",43)]

{- | Binary operators.

     Sc>>** is not St>>raisedTo: since in Sc (0 ** 0) is 1 and in St (0 raisedTo: 0) is an error
     Smalltalk modulo is rem: but use % here.

> map snd binop
-}
binop :: [(String, Int)]
binop =
  [("+",0),("-",1),("*",2),("//", 3), ("/",4),("%",5),("/=",7),("<",8),(">",9)
  ,("<=",10),(">=",11),("min:",12),("max:",13)
  ,("bitAnd:",14),("bitOr:",15)
  ,("lcm:",17),("gcd:",18),("roundTo:",19)
  ,("truncateTo:",21), ("hypot:", 23), ("raisedTo:",25)
  ,("bitShiftLeft:",26),("bitShiftRight:",27)
  ,("amClip:",40),("scaleNeg:", 41),("clip2:",42),("fold2:",44)]

ugen :: [String]
ugen = Spl.ugen_list_core ++ Spl.ugen_list_ext

{-
> selectorToSpecialIndex uop
> selectorToSpecialIndex binop
-}
selectorToSpecialIndex :: [(String, Int)] -> String
selectorToSpecialIndex =
  let f (sel, ix) = printf "#%s -> %d" sel ix
  in intercalate ". " . map f

is_osc :: Record.U -> Bool
is_osc u = (Record.u_num_inputs u > 0) && not (Record.u_is_filter u)

main :: IO ()
main = do
  Som.som_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/som/Sc3/Ugen/" ugen
  St.st_sc3_gen_bindings_wr "/home/rohan/sw/stsc3/st/Sc3-Ugen-Bindings.st" uop binop ugen
  Js.js_sc3_gen_bindings_wr True "/home/rohan/sw/jssc3/js/sc3-bindings.ts" Js.js_sc3_uop Js.js_sc3_binop ugen
  let col = map u_lookup_cs_err ugen
      flt = filter Record.u_is_filter col
      osc = filter is_osc col
  writeFile "/home/rohan/sw/stsc3/st/Sc3-Ugen-Filter.st" (St.st_filter_methods flt ++ St.st_first_input_methods osc)

-- * Sc

sc_wr :: IO ()
sc_wr = do
  let u_fc = map u_lookup_cs_err (filter (`notElem` Sc.sc_filter_constructor_ignore_list) ugen)
      u_ir = map u_lookup_cs_err (filter (`notElem` Sc.sc_implicit_rate_ignore_list) ugen)
      --u_fm = map u_lookup_cs_err (filter (`notElem` Sc.sc_filter_method_ignore_list) ugen)
      --u_fi = map u_lookup_cs_err (filter (`notElem` Sc.sc_first_input_ignore_list) ugen)
  --writeFile "/home/rohan/sw/stsc3/sc/FilterMethods.sc" (Sc.sc_filter_methods (filter Record.u_is_filter u_fm))
  --writeFile "/home/rohan/sw/stsc3/sc/OscillatorMethods.sc" (Sc.sc_first_input_methods (filter is_osc u_fi))
  writeFile "/home/rohan/sw/stsc3/sc/FilterConstructors.sc" (Sc.sc_filter_constructors (filter Record.u_is_filter u_fc))
  writeFile "/home/rohan/sw/stsc3/sc/ImplicitRateConstructors.sc" (Sc.sc_implicit_rate_constructors (filter (isNothing . Record.ugen_filter) u_ir))

{-
-}
