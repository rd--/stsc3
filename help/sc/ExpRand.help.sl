# ExpRand -- random number generator

_ExpRand(lo, hi)_

Generates a single random float value in an exponential distributions from _lo_ to _hi_.

	var n = 5;
	var o = SinOsc({ ExpRand(110, 220) } ! n, 0) * ({ ExpRand(0.05, 0.10) } ! n);
	Splay2(o)

