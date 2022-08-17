# NRand

_NRand(lo, hi, n)_

Generates a single random float value in a sum of n uniform distributions from lo to hi.

- n = 1 : uniform distribution, same as Rand
- n = 2 : triangular distribution
- n = 3 : smooth hump

As n increases, distribution converges towards gaussian.

	var n = MouseX(1, 9, 0, 0.2).rounded;
	{
		var freq = NRand(200, 10000, n);
		var dur =  (1 / freq) * 7500;
		FSinOsc(freq, 0) * Ln(0.2, 0, dur)
	}.dup(15).splay2
