(* police state ; jmcc *)
var n = 4; (* number of sirens *)
CombL(
	{
		EqPan(
			SinOsc(
				MulAdd(
					SinOsc(0.1.Rand + 0.02, 2 * pi.Rand),
					600.Rand,
					1000 + 300.Rand2
				),
				0
			),
			1.Rand2
		) * LfNoise2(100 + 20.Rand2) * 0.1
	} !+ 4 + Mul(
		LfNoise2(MulAdd(LfNoise2([0.4, 0.4]), 90, 620)),
		MulAdd(LfNoise2([0.3, 0.3]), 0.15, 0.18)
	),
	0.3, 0.3, 3
) * 0.5
