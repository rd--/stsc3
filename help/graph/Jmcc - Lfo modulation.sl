(* lfo modulation of pulse waves and resonant filters ; jmcc *)
CombL(
	Rlpf(
		LfPulse(MulAdd(SinOsc(0.05, 0), 80, 160), 0, 0.4) * 0.05,
		MulAdd(SinOsc([0.6, 0.7], 0), 3600, 4000),
		0.2
	),
	0.3,
	[0.2, 0.25],
	2
)
