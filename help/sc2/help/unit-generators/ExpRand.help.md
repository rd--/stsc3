# ExpRand

_ExpRand(lo, hi)_

Generates a single random float value in an exponential distributions from lo to hi.

	var n = 5;
	var o = SinOsc({ ExpRand(110, 220) }.dup(n), 0) * ({ ExpRand(0.05, 0.10) }.dup(n));
	Splay2(o)

