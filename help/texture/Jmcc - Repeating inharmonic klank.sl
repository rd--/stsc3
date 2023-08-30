(* repeating inharmonic klank ; jmcc #6 ; graph rewrite *)
var n = 4;
{
	var s = Decay(Dust(0.8) * 0.004, 3.4) * LfSaw(Rand(0, 40), 0);
	var p = 8;
	{
		RingzBank(
			s,
			{ Rand(80, 10000) } ! p,
			nil,
			{ Rand(0.4, 4.4) } ! p
		)
	} ! 2
}.overlap(8, 8, n)
