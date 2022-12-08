# HPZ2 - two zero fixed highpass

_HPZ2(in)_

A special case fixed filter. Implements the formula _out(i) = 0.25 * (in(i) - (2*in(i-1)) + in(i-2))_.

Compare:

	WhiteNoise() * 0.1

and:

	HPZ2(WhiteNoise()) * 0.1

