;; what was I thinking? ; jmcc
var z = Rlpf(
	Pulse(
		MulAdd(SinOsc(4, 0), 1, 80).max(
			Decay(LfPulse(0.1, 0, 0.05) * Impulse(8, 0) * 500, 2)
		),
		MulAdd(LfNoise1(0.157), 0.4, 0.5)
	) * 0.04,
	MulAdd(LfNoise1(0.2), 2000, 2400),
	0.2
);
var y = z * 0.6;
z +  [
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1) +
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1)
	,
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1) +
	CombL(y, 0.06, MulAdd(LfNoise1(0.3.Rand), 0.025, 0.035), 1)
]

;; what was i thinking? (jmcc) #2
var i = LfPulse(0.1, 0, 0.05) * Impulse(8, 0) * 500;
var f = (SinOsc(4, 0) + 80).max(Decay(i, 2));
var p = Pulse(f, LfNoise1(0.157) * 0.4 + 0.5) * 0.04;
var z = Rlpf(p, LfNoise1(0.2) * 2000 + 2400, 0.2) * 0.25;
var c = { :i | CombL(i, 0.06, LfNoise1(0.3.Rand) * 0.025 + 0.035, 1) };
var y = z * 0.6;
z + { [y, y].collect(c).sum }.dup
