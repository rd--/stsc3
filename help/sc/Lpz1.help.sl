# Lpz1 -- two point average filter

_Lpz1(in)_

A special case fixed filter. Implements the formula _out[i] = 0.5 * (in[i] + in[i - 1])_ which is a two point averager.

Compare:

	WhiteNoise() * 0.1

and:

	Lpz1(WhiteNoise() * 0.1)

* * *

See also: _Hpz1_, _Hpz2_, _Lpz2_
