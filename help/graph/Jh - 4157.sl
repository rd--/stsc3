;; jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit)
var t = Impulse(0.1, 0);
var sinosc = { :freq |
	(Phasor(t, freq * SampleDur(), 0, 1, 0) * 2 * pi).Sin
};
var k = 120; (* 160 ; udp *)
var b = TRand(2, 2.25, t);
var n = TRand(0.002, 0.02, t);
var f0 = TRand(90, 180, t);
var fMul = b ** k.series(0, n);
var e = Decay2(t, 1, 10) * 0.1;
Splay(sinosc(f0 * fMul), 1, e, 0, true)
