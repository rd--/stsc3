# OneZero -- one zero filter

_OneZero(in, coef)_

A one zero filter. Implements the formula _out(i) = ((1 - abs(coef)) * in(i)) + (coef * in(i-1))_.

- in: input signal to be processed
- coef: feed forward coefficient. +0.5 makes a two point averaging filter (see also LPZ1), -0.5 makes a differentiator (see also HPZ1), +1 makes a single sample delay (see also Delay1), -1 makes an inverted single sample delay.

Low pass:

	OneZero(WhiteNoise() * 0.2, 0.5)

High pass:

	OneZero(WhiteNoise() * 0.2, -0.5)

Modulate coeficient:

	OneZero(WhiteNoise() * 0.2, Ln(-0.5, 0.5, 10))

