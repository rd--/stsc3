# LPZ1 - two point average filter

_LPZ1(in)_

A special case fixed filter. Implements the formula _out(i) = 0.5 * (in(i) + in(i-1))_ which is a two point averager.

Compare:

	WhiteNoise() * 0.1

and:

	LPZ1(WhiteNoise() * 0.1)

