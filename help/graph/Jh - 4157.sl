(* jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit) *)
var tr = Impulse(0.1, 0);
var sinOsc = { :freq |
	(Phasor(tr, freq * SampleDur(), 0, 1, 0) * 2 * pi).Sin
};
var k = 160;
var b = TRand(2, 2.25, tr);
var n = TRand(0.002, 0.02, tr);
var f0 = TRand(90, 180, tr);
var fMul = b ^ k.arithmeticSeries(0, n);
var e = Decay2(tr, 1, 10) * 0.1;
sinOsc(f0 * fMul).Splay * e
