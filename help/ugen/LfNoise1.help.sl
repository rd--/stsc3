(* LfNoise1 *)
(1 .. 5).collect { :n |
	EqPan2(
		SinOsc(n * 100, 0) * (LfNoise1(6) + Line(1, -1, 30)).Max(0),
		Rand(-1, 1)
	)
}.Mix * 0.025

(* LfNoise1 *)
var freq = LinLin(LfNoise1(1), -1, 1, 220, 440);
SinOsc(freq, 0) * 0.1
