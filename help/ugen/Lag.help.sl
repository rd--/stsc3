(* Lag *)
var f1 = LinLin(SinOsc(0.05, 0), -1, 1, 220, 440);
var o1 = SinOsc(f1, 0);
var f2 = Lag(f1, 1);
var o2 = SinOsc(f2, 0);
[o1, o2] * 0.1
