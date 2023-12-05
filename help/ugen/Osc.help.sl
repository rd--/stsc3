(* Osc ; sineTable *)
var c = 4096.sineTable(1 / (1 .. 3), [0]).normalize.asWavetable.asLocalBuf;
Osc(c, MouseX(222, 555, 1, 0.2), 0) * 0.1

(* Osc ; sineTable *)
var c = 4096.sineTable(1 / (1 .. 128), [0]).normalize.asWavetable.asLocalBuf;
{
	Osc(
		c,
		SinOsc(
			{ Rand(3.3, 4.4) } ! 6,
			0
		) * 0.01 + 1 * [82 123 196 147 41 55],
		0
	).Sum
} !^ 3 / 17
