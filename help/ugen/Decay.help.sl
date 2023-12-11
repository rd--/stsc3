(* Decay ; as envelope *)
var tr = LfPulse(MouseX(3, 21, 1, 0.2), 0.2, 0.0004);
Decay(tr, { TRand(0.01, 0.35, tr) } ! 2) * SinOsc({ TRand(500, 700, tr) } ! 2, 0) * 0.1

(* ---- Dust ; as envelope ; applicative form *)
var lfo = { :freq :lo :hi |
	Range(LfNoise2(freq), lo, hi)
};
AllpassN(
	Bpf(
		PinkNoise() * Decay(
			Dust([3 / 5, 5 / 7]) * 1 / 4,
			lfo(1 / 3, 1 / 5, 5 / 7)
		),
		lfo(1 / 3, 700, 2300),
		lfo(1 / 3, 1 / 9, 3)
	),
	1 / 5,
	1 / 5,
	lfo(1 / 3, 1 / 3, 3)
)

(* ---- Dust ; as envelope ; left-to-right *)
Dust(1).Mul(0.25).Decay(0.2).Mul(PinkNoise()).AllpassN(0.2, 0.2, 3)

(* ---- Dust ; as envelope ; left-to-right *)
var lfo = { :freq :lo :hi | LfNoise2(freq).Range(lo, hi) };
Dust([3 / 5, 5 / 7])
	.Mul(1 / 4)
	.Decay(lfo(1 / 3, 1 / 5, 5 / 7))
	.Mul(PinkNoise())
	.Bpf(lfo(1 / 3, 700, 2300), lfo(1 / 3, 1 / 9, 3))
	.AllpassN(1 / 5, 1 / 5, lfo(1 / 3, 1 / 3, 3))
