;; police state ; jmcc
var n = 4; (* number of sirens *)
CombL(
	{
		Pan2(
			SinOsc(
				MulAdd(SinOsc(0.1.rand + 0.02, 2 * pi.rand), 600.rand, 1000 + 300.rand2),
				0
			),
			1.0.rand2,
			LfNoise2(100 + 20.0.rand2) * 0.1
		)
	}.dup(4).sum
	+ MulAdd(LfNoise2(MulAdd(LfNoise2([0.4, 0.4]), 90, 620)), MulAdd(LfNoise2([0.3, 0.3]), 0.15, 0.18), 0),
	0.3, 0.3, 3
) * 0.5

;; police state (jmcc) #2
var node = {
	var f = SinOsc(Rand(0.02, 0.12), Rand(0, 2 * pi)) * Rand(0, 600) + 1000 + Rand(-300, 300);
	Pan2(SinOsc(f, 0) * LfNoise2(100 + Rand(-20, 20)) * 0.1, Rand(-1, 1), 1)
};
var e = LfNoise2(LfNoise2([0.4, 0.4]) * 90 + 620) * (LfNoise2([0.3, 0.3]) * 0.15 + 0.18);
CombL(node.dup(4).sum + e, 0.3, 0.3, 3) * 0.5

;; police state ; jmcc ; keywords
var n = 4; (* number of sirens *)
var node = {
	Pan2(
		in: SinOsc(
			freq: SinOsc(
				freq: 0.1.rand + 0.02,
				phase: 2 * pi.rand
			) * 600.rand + 1000 + 300.rand2,
			phase: 0
		),
		pos: 1.0.rand2,
		level: LfNoise2(freq: 100 + 20.0.rand2) * 0.1
	)
};
var e = LfNoise2(freq: LfNoise2(freq: [0.4, 0.4]) * 90 + 620) * (LfNoise2(freq: [0.3, 0.3]) * 0.15 + 0.18);
CombL(
	in: node.dup(4).sum + e,
	maxdelaytime: 0.3,
	delaytime: 0.3,
	decaytime: 3
) * 0.5
