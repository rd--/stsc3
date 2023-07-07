# Brz2 -- two zero fixed midcut

_Brz2(in)_

A special case fixed filter. Implements the formula _out(i) = 0.5 * (in(i) + in(i-2))_.

This filter cuts out frequencies around 1/2 of the Nyquist frequency.

Compare:

	WhiteNoise() * 0.1

and:

	Brz2(WhiteNoise() * 0.1)

