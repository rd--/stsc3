(* LatoocarfianC ; randomly modulate all params *)
LatoocarfianC(
	SampleRate() / 4,
	LfNoise2(2) * 1.5 + 1.5,
	LfNoise2(2) * 1.5 + 1.5,
	LfNoise2(2) * 0.5 + 1.5,
	LfNoise2(2) * 0.5 + 1.5,
	0.5,
	0.5
) * 0.1

(* LatoocarfianC ; texture *)
{ :tr |
	var freq = TRand(400, SampleRate() / 3, tr);
	var a = TRand(-3, 3, tr);
	var b = TRand(-3, 3, tr);
	var c = TRand(0.5, 1.5, tr);
	var d = TRand(0.5, 1.5, tr);
	SinOsc(freq, 0) * 0.05 + Pan2(LatoocarfianC(freq, a, b, c, d, 0.5, 0.5), TRand(-1, 1, tr), 0.05)
}.OverlapTexture(1, 4, 8).Mix
