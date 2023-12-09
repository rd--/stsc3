(* rocks on rails ; jmcc ; graph variant ; requires=LinRand *)
var p = 20; (* number of partials *)
var n = 4; (* number of overlapping events *)
{ :tr |
	EqPan2(
		DynRingzBank( (* p resonant modes *)
			Resonz(
				Dust(100) * 0.04, (* excitation *)
				XLine(3000, 300, 8), (* sweep filter down *)
				0.2 (* band width ratio *)
			),
			{ 200 + TLinRand(0, 3000, 0, tr) } ! p, (* resonant frequencies *)
			[1], (* amplitudes *)
			{ 0.2 + TRand(0, 1, tr) } ! p (* ring times *)
		),
		Line(TRand(-1, 1, tr), TRand(-1, 1, tr), 8) (* sweep pan *)
	)
}.OverlapTexture(2, 3, n).Mix * 0.5
