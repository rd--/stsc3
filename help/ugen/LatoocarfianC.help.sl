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
	var freq = Rand(tr, 400, SampleRate() / 3);
	var a = Rand(tr, -3, 3);
	var b = Rand(tr, -3, 3);
	var c = Rand(tr, 0.5, 1.5);
	var d = Rand(tr, 0.5, 1.5);
	SinOsc(freq, 0) * 0.05 + Pan2(LatoocarfianC(freq, a, b, c, d, 0.5, 0.5), Rand(tr, -1, 1), 0.05)
}.OverlapTexture(1, 4, 8).Mix
