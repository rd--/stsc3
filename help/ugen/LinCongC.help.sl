;; LinCongC ; texture
OverlapTexture({ :tr |
	var freq = SampleRate() / 2;
	var m = TIRand(0, 1000000, tr);
	var a = TIRand(1, 2000, tr);
	var c = TIRand(1, 30000, tr);
	LinCongC(freq, a, c, m, { TIRand(0, m, tr) } ! 2) * 0.1
}, 1, 2, 4)
