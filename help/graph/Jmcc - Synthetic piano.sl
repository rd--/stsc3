(* synthetic piano (jmcc) #3 *)
{
	var n = Rand(36, 90);
	var e = Decay2(Impulse(Rand(0.1, 0.5), 1.Rand) * 0.1, 0.008, 0.04);
	var z = (1 .. 3).collect { :i |
		var dt = 1 / (n + [-0.05 0 0.04][i]).MidiCps;
		CombL(LfNoise2(3000) * e, dt, dt, 6)
	};
	EqPan2(z.Sum, n - 36 / 27 - 1)
} !> 3
