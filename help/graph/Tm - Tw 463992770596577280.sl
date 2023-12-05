(* https://twitter.com/thormagnusson/status/463992770596577280 (tm) *)
var k = 9 + 19.atRandom;
{
	(1 .. k).collect { :x |
		var e = LfNoise2(0.5) * Line(0, 0.1, Rand(0, 99)) / (x * 0.2);
		SinOsc(30 * x + LinLin(LfNoise2(0.1), -1, 1, -2, 2), 0) * e
	}.Sum
} ! 2
