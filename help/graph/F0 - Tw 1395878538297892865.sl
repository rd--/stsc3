(* f0 ; <https://twitter.com/redFrik/status/1395878538297892865> *)
var t = Impulse(5, 0);
var g = [3 2];
var e = LagUd(t, 0.001, SinOscFb(g / 99, 0) + 1.08);
var m = [24 0 3 5 7 10 36] +.x [36 48 36 33 60 72];
var o = SinOscFb(Demand(t, 0, Dseq(inf, m.MidiCps) / g), SinOscFb(0.02, 0)) * e;
var c = Rlpf(o, 3 ^ SinOscFb(0.04, 0) + e * 2000, 3 ^ SinOscFb(g / 9, 0) / 3) * 4;
(CombC([c.Tanh, c], 1, 1 / [2 3], [4 5]).Mix / 8 + c).Tanh * 0.1
