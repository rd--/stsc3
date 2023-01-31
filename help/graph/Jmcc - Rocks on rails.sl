;; rocks on rails ; jmcc ; graph variant ; requires=TLinRand
var p = 20; (* number of partials *)
var n = 4; (* number of overlapping events *)
OverlapTexture({ :tr |
	EqPan2(
		DynRingzBank( (* p resonant modes *)
			Resonz(
				Dust(100) * 0.04, (* excitation *)
				XLn(3000, 300, 8), (* sweep filter down *)
				0.2 (* band width ratio *)
			),
			{ 200 + TRand(0, 3000, tr) } ! p, (* resonant frequencies (TLinRand) *)
			[1], (* amplitudes *)
			{ 0.2 + TRand(0, 1, tr) } ! p (* ring times *)
		),
		Ln(TRand(-1, 1, tr), TRand(-1, 1, tr), 8) (* sweep pan *)
	)
}, 2, 3, n) * 0.5
