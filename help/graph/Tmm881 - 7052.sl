(* ---- https://scsynth.org/t/7052 ; tmm881 ; graph texture rewrite ; requires=XFadeTexture *)
{ :tr |
	var freqMul = TRand(0.5, 2, tr);
	var env = {
		var freq = Choose(tr, [10, 5, 2.5]) * freqMul;
		Perc([LfClipNoise(freq), LfNoise0(freq)], 0.01, 1, -4)
	};
	var snd = [
		SinOsc([[48, 49]] * freqMul, 0) * env(),
		Saw(48 * 16 * freqMul) * env(),
		Pulse(48 * 32 * freqMul, 0.5) * env()
	].Sum.transposed.Mix;
	var rev = FreeVerb(snd, 0.33, 0.5, 0.5) + snd;
	CombC(rev, 0.2, 0.2, 1) + rev * 0.05
}.XFadeTexture(9, 2)
