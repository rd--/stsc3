(* f0 ; <https://twitter.com/redFrik/status/1395519538008141835> *)
var c = DmdFor(0.004, 0, Dseries(inf, 1, [1, 2]) % DmdFor(8.192, 0, Dseq(inf, (1 .. 6) * 75)));
var d = Hpf(MantissaMask(c, 3),5);
var f = { :x | SinOscFb(x, 0).Max(0) };
var p = [250, 200] * Lag(DmdFor(4.096, 0, Dseq(inf, [4, 6, 5, 5, 5, 5, 3] / 4)), 0.1);
var o = SinOscFb(p, f(0.08)) * f(1 / [99, 60]) / 3;
Lpf((d.Sin + (SinOscFb(63, f(0.8)) * d) / 9 + o).Tanh / 2, 10000) * 0.2
