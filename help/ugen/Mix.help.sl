(* Mix ; rule based mixing ; system preference *)
var n = 9;
{ :tr |
	var ping = SinOsc(2222, 0) * Decay2(tr, 0.01, 0.2);
	var sine = SinOsc(TRand(22, 333, tr), 0);
	EqPan2(
		[ping, sine],
		[-1, TRand(-1, 1, tr)]
	).Sum * [0.1, 0.05]
}.OverlapTexture(n, 0, n).Mix

(* Mix ; !> is ! then Mix *)
{
	EqPan2(
		SinOsc(Rand(22, 333), 0),
		Rand(-1, 1)
	) * Rand(0.01, 0.05)
} !> 9

(* Mix ; simple '2×2→1×4' rule *)
{
	SinOsc(
		{ Rand(111, 333) } ! 2,
		0
	) / Rand(7, 23)
} !> 2

