(* Demand *)
var n = Dseq(inf, [0 2 4 5 7 9 11 12]) + (TRand(3, 6, Dust(9)).RoundTo(0) * 12);
var t = Impulse(10, 0);
var f = Demand(t, 0, n.MidiCps);
var o = SinOsc([f, f + 0.7], 0);
o.cubed.cubed * 0.1

(* Demand *)
var n = Diwhite(inf, 60, 72);
var t = Impulse(10, 0);
var f = Demand(t, 0, n.MidiCps);
var o = SinOsc([f, f + 0.7], 0);
o.cubed.cubed * 0.1
