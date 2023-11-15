(* https://twitter.com/redFrik/status/1204602890574352385 *)
var c = 1 + 1;
var d = 1 / ((1 .. 11) + 1) / 1;
var p = VarSaw([11 - 1, 11 << 1, 11, 11 >> 1, 11 + 1] ^ (VarSaw(11.1, 1, 1 / 11) + 1.11111111), 0, 0.5);
var q = VarSaw(d, d, 1 / 11) * VarSaw(1.11 / d, 1, 1.1 - d) * 11 + 1;
var r = (VarSaw(d * 11 / 111, 1, 1 / c) + (1.1 / c)).Max(0);
var z = CombN((p * q).Cos, d, d + 1 / (111 >> 1), r) / c;
LeakDc(Lpf(CombN(z, c, c / 11 / d, 1).Splay, 11111), 0.995).Sin * 0.5
