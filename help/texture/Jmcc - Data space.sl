(* data space ; jmcc ; #2 *)
{
	var dt = Rand(0.25, 0.35);
	var osc = { :n :m |
		var e = MulAdd(
			LfPulse(m.Rand, 0, 1.Rand),
			8000.Rand,
			2000.Rand
		);
		LfPulse(n.Rand, 0, 1.Rand) * e
	};
	var freq = osc(200.0, 40.0) + osc(20.0, 4.0) + osc(20.0, 4.0);
	CombL(
		EqPan(
			LfPulse(freq, 0, 0.5),
			LfNoise0(3.Rand) * 0.8
		) * 0.04, dt, dt, 3
	)
}.overlap(6, 1, 4)
