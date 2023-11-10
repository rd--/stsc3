(* GrayNoise *)
{ :tr |
	var amp = LfPulse(4, 0, 0.1) * 0.002;
	var exc = Lpz1({ GrayNoise() } ! 2 * amp);
	Ringz(exc, { Rand(tr, 80, 400) } ! 4, 1)
}.OverlapTexture(4, 4, 2)
