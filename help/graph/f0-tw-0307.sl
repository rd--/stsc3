;; https://sccode.org/1-4Qy ; f0 ; 0307
var a = 2 + Blip(3 / [8, 9], 2).rounded;
var c = a ** Lag(a, 0.1);
var o = Blip(DmdFor(1 / [9, 8], 0, Seq(inf, [65, 86, 86, 86, 70].midiCps) / a), c) * (c + 5);
CombN(o, 0.2, 0.2, 1).tanh * 0.15

(*
'AVVVF'.ascii = [65, 86, 86, 86, 70]
*)
