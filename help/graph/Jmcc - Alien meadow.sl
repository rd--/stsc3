;; alien meadow (jmcc) #6
OverlapTexture({ :tr |
	var z = TRand(0, 5000, tr);
	var f = SinOsc(TRand(0, 20, tr), 0) * (0.1 * z) + z;
	var a = SinOsc(TRand(0, 20, tr), 0) * 0.05 + 0.05;
	Pan2(SinOsc(f, 0), TRand(-1, 1, tr), a)
}, 6, 2, 6)

;; alien meadow (jmcc) #6 ; left-to-right
OverlapTexture({ :tr |
	var trRand = { :lo :hi | TRand(lo, hi, tr) };
	var z = trRand(0, 5000);
	var f = SinOsc(trRand(0, 20), 0).MulAdd(0.1 * z, z);
	var a = SinOsc(trRand(0, 20), 0).MulAdd(0.05, 0.05);
	SinOsc(f, 0).Pan2(trRand(-1, 1), a)
}, 6, 2, 6)
