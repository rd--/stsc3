# LinRand -- random number generator

_LinRand(lo, hi, minmax)_

Generates a single random float value in linear distribution from _lo_ to _hi_, skewed towards _lo_ if _minmax_ `< 0`, otherwise skewed towards _hi_.

	var minmax = MouseX(0, 1, 0, 0.2);
	{
		var freq = LinRand(200, 10000, minmax);
		var dur = (1 / freq) * 7500;
		FSinOsc(freq, 0) * Ln(0.2, 0, dur)
	} !^ 15

* * *

See also: IRand, NRand, Rand, Rand2
