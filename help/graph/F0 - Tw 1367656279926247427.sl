(* f0 <https://twitter.com/redFrik/status/1367656279926247427> *)
var b = [20 15 18 19 4 1 7 11 22 -125 -188 12 12];
var f = { :i | SinOsc(i, b).RoundTo(1) };
var t = SinOsc(SinOsc(0.01, 0).RoundTo(1) + 10, b);
var c = GrainSin(1, t, 0.85, f(0.4) + f(0.1) + 2 ^ f(1 / b) + 1 * 99, 0, -1, 512) / 5;
var x = Hpf(SinOsc(2, c * pi) + (PitchShift(c, 0.5, f(1) * f(0.1) + 2, 0.05, 1) * 0.15), 5);
var y = SinOsc(50, 0) * Hpf(SinOsc(1, [0, 1]).RoundTo(1), 1) / 2;
(x + y).Splay.Tanh
