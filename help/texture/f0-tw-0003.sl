;; https://sccode.org/1-4Qy ; f0 ; 0003
{ :d |
	var t = SinOsc(Rand(99, 999), 0).abs;
	var o = Formlet(TDmdFor(t, 0, t), LinRand(0, 4000, 0), t, 1 - t);
	Release(o ! 2, 0, d, 39);
}.playEvery { 9.randomFloat + 1 }
