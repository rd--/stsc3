(* tw 0121 (f0) *)
var z = SinOsc(1 / [8, 7], 0) * SinOsc(1 / 30, 0) * 9;
var l = ((0 .. 7) * 6 + 56).asLocalBuf;
var m = SinOsc(1 / [4, 3], 0);
var o = SinOsc(BufRd(1, l, z, 0, 1).MidiCps, 0) * m;
CombN(o, 1, 1 / [6, 5], 9).Tanh * 0.1
