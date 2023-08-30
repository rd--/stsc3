(* pulsing bottles ; jmcc *)
var n = 6; (* number of 'bottles' *)
{
	EqPan2(
		Resonz(
			WhiteNoise(),
			400 + LinRand(0, 7000, 0),
			0.01
		),
		SinOsc(0.1 + 0.4.Rand, (2 * pi).Rand)
	) * LfPulse(4 + 10.Rand, 0, 0.7.Rand) * 0.8 / n
} !+ n
