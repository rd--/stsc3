;; rocks on rails ; jmcc #2
var p = 20; (* number of partials *)
var n = 4; (* number of overlapping events *)
{
	Pan2(
		RingzBank( (* p resonant modes *)
			Resonz(
				Dust(100) * 0.04, (* excitation *)
				XLn(3000, 300, 8), (* sweep filter down *)
				0.2 (* band width ratio *)
			),
			{ 200 + LinRand(0, 3000, 0) } ! p, (* resonant frequencies *)
			nil, (* amplitudes default to 1.0 *)
			{ 0.2 + Rand(0, 1) } ! p (* ring times *)
		),
		Ln(Rand(-1, 1), Rand(-1, 1), 8), (* sweep pan *)
		1
	)
}.overlap(2, 3, n)
