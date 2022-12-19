;; https://twitter.com/thormagnusson/status/463992770596577280 (tm)
var k = 9 + 19.atRandom;
var f = { :x |
	var e = LfNoise2(0.5) * Ln(0, 0.1, Rand(0, 99)) / (x * 0.2);
	SinOsc(30 * x + LinLin(LfNoise2(0.1), -1, 1, -2, 2), 0) * e
};
{ (1 .. k).collect(f).sum } ! 2
