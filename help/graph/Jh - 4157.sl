(* jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit) *)
var tr = Impulse(0.1, 0);
var sinosc = { :freq |
	(Phasor(tr, freq * SampleDur(), 0, 1, 0) * 2 * pi).Sin
};
var k = 160;
var b = TrRand(tr, 2, 2.25);
var n = TrRand(tr, 0.002, 0.02);
var f0 = TrRand(tr, 90, 180);
var fMul = b ^ k.arithmeticSeries(0, n);
var e = Decay2(tr, 1, 10) * 0.1;
Splay(sinosc(f0 * fMul), 1, e, 0, true)
