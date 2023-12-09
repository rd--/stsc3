(* trinkets (1 & 2) (jmcc) ; http://www.iamas.ac.jp/~aka/dspss2004/materials/ *)
var amp = 0.2;
var gate = 1;
var z = 0.0, a = 0.1, b = 0.2, c = 0.4;
var tr = [Dust(4), Impulse(16, 0)].atRandom;
var x = SinOsc(TRand([400, 2000].atRandom, 12000, tr), 0) * Decay2(tr, 0.002, 0.04);
x := EqPan(x, TRand(-1, 1, tr));
x := CombN(x, 0.2, { Rand(0.04, 0.2) } ! 4, 2);
x * Linen(gate, 0.1, amp, 0.3, 2)
