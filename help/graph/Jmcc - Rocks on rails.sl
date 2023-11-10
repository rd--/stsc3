(* rocks on rails ; jmcc ; graph variant ; requires=LinRand *)
var p = 20; (* number of partials *)
var n = 4; (* number of overlapping events *)
{ :tr |
	EqPan2(
		DynRingzBank( (* p resonant modes *)
			Resonz(
				Dust(100) * 0.04, (* excitation *)
				XLn(3000, 300, 8), (* sweep filter down *)
				0.2 (* band width ratio *)
			),
			{ 200 + LinRand(tr, 0, 3000, 0) } ! p, (* resonant frequencies *)
			[1], (* amplitudes *)
			{ 0.2 + Rand(tr, 0, 1) } ! p (* ring times *)
		),
		Ln(Rand(tr, -1, 1), Rand(tr, -1, 1), 8) (* sweep pan *)
	)
}.OverlapTexture(2, 3, n) * 0.5
