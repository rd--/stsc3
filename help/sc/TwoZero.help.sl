# TwoZero -- two zero filter

_TwoZero(in, freq, radius)_

A two zero filter.

- in: input signal to be processed
- freq: frequency of zero angle.
- radius: radius of zero.

Modulate frequency:

	TwoZero(WhiteNoise() * 0.125, XLn(20, 20000, 8), 1)
