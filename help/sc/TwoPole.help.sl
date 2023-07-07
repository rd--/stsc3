# TwoPole -- two pole filter

_TwoPole(in, freq, radius)_

A two pole filter. This provides lower level access to setting of pole location. For general purposes Resonz is better.

- in: input signal to be processed
- freq: frequency of pole angle.
- radius: radius of pole. Should be between 0 and +1

Fixed frequency:

	TwoPole(WhiteNoise() * 0.005, 2000, 0.95)

Modulate frequency:

	TwoPole(WhiteNoise() * 0.005, XLn(800, 8000, 8), 0.95)

