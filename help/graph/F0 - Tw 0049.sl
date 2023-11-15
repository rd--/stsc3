(* tw 0049 (f0) *)
var o0 = LfSaw(3, 0) * 9 + 99;
var s0 = Sweep(0, [3 .. 9]) % o0;
var s1 = (Sweep(0, [31 .. 1] * -1 + 34) % 128).BitAnd(s0);
var o1 = SinOsc((s1 + 33).MidiCps, 0) * pi;
SinOsc(9, o1).Splay / 9
