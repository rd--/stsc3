# LinRand

_LinRand(lo, hi, minmax)_

Generates a single random float value in linear distribution from lo to hi, skewed towards lo if minmax < 0, otherwise skewed towards hi.

	var minmax = MouseX(0, 1, 0, 0.2);
	{
		var freq = LinRand(200, 10000, minmax);
		var dur =  (1 / freq) * 7500;
		FSinOsc(freq, 0) * Ln(0.2, 0, dur)
	}.dup(15).splay2

