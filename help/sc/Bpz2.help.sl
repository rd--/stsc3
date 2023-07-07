# Bpz2 -- two zero fixed midpass

_Bpz2(in)_

A special case fixed filter. Implements the formula _out(i) = 0.5 * (in(i) - in(i-2))_

This filter cuts out 0 Hz and the Nyquist frequency.

Compare:

	WhiteNoise() * 0.1

and:

	Bpz2(WhiteNoise() * 0.1)

