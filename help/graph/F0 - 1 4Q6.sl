(* https://sccode.org/1-4Q6 ; f0 ; risset ; requires=LinRand *)
{ :tr |
	var pan = TRand(-1, 1, tr);
	var freq = TRand(90, 2000, tr);
	var amp = TRand(0.1, 0.2, tr);
	var dur = TLinRand(0.5, 9, 1, tr);
	var ampArray = [1 0.67 1 1.8 2.67 1.67 1.46 1.33 1.33 1 1.33];
	var durArray = [1 0.9 0.65 0.55 0.325 0.35 0.25 0.2 0.15 0.1 0.075];
	var freqArray = [0.56 0.56 0.92 0.92 1.19 1.7 2 2.74 3 3.76 4.07];
	var detuneArray = [0 1 0 1.7 0 0 0 0 0 0 0];
	var src = (1 .. 11).collect { :i |
		var env = Perc(tr, 0.005, dur * durArray[i], -4.5) * ampArray[i];
		SinOsc(freq * freqArray[i] + detuneArray[i], 0) * amp * env
	};
	EqPan2(src.Sum, pan)
}.OverlapTexture(9, 0, 7).Mix * 0.1
