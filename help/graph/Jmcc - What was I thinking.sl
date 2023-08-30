(* what was I thinking? ; jmcc *)
var z = Rlpf(
	Pulse(
		Decay(LfPulse(0.1, 0, 0.05) * Impulse(8, 0) * 500, 2).Max(
			SinOsc(4, 0) + 80
		),
		LfNoise1(0.157) * 0.4 + 0.5
	) * 0.04,
	LfNoise1(0.2) * 2000 + 2400,
	0.2
);
var y = z * 0.6;
{
	{
		CombL(y, 0.06, LfNoise1(Rand(0, 0.3)) * 0.025 + 0.035, 1)
	} !+ 2
} ! 2 + z
