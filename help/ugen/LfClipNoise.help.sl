(* ---- LfClipNoise ; process (Eval) *)
{
	var freq = Rand(47, 69) * [1, 1.1];
	var mul = {
		Perc(LfClipNoise(Rand(3, 7)), 0.01, 1, -4) * 0.1
	} ! 2;
	var snd = [
		SinOsc(freq * [1, 2], 0),
		Saw(freq * [2, 3]),
		Pulse(freq * [3, 4], 0.5)
	] * mul;
	FreeVerb(snd.Sum, 0.33, 0.5, 0.5)
}.overlap(7, 7, 3)
