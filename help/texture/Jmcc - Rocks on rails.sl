(* rocks on rails ; jmcc #2 *)
var p = 20; (* number of partials *)
var n = 4; (* number of overlapping events *)
{
	EqPan(
		RingzBank( (* p resonant modes *)
			Resonz(
				Dust(100) * 0.04, (* excitation *)
				XLine(3000, 300, 8), (* sweep filter down *)
				0.2 (* band width ratio *)
			),
			{ 200 + LinRand(0, 3000, 0) } ! p, (* resonant frequencies *)
			nil, (* amplitudes default to 1.0 *)
			{ 0.2 + 1.Rand } ! p (* ring times *)
		),
		Line(1.Rand2, 1.Rand2, 8) (* sweep pan *)
	)
}.overlap(2, 3, n)
