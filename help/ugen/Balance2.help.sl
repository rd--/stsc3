(* Balance2 *)
Balance2(LfSaw(44, 0), Pulse(33, 0.5), SinOsc(0.5, 0), 0.1)

(* Balance2 *)
{ Balance2(LfSaw(44, 0), Pulse(33, 0.5), SinOsc(Rand(0.25, 0.75), 0), 0.05) } !+ 2

(* Balance2 ; with control rate position input *)
{ Balance2(LfSaw(44, 0), Pulse(33, 0.5), SinOsc(Rand(0.25, 0.75), 0).kr, 0.1) } !+ 2

(* Balance2 *)
var o = SinOsc([440, 550], 0);
Balance2(o.first, o.second, LfNoise1(4), 0.1)

(* Balance2 *)
var o = SinOsc(440, 0);
Balance2(o, o, SinOsc(0.2, 0), 0.1)

(* Balance2 *)
var o = SinOsc(440, 0);
Balance2(o, o, MouseX(-1, 1, 0, 0.2), 0.2)
