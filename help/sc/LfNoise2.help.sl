# LfNoise2 -- quadratic noise

_LfNoise2(freq)_

Generates quadratically interpolated random values at a rate given by the nearest integer division of the sample rate by the freq argument.

- freq: approximate rate at which to generate random values.

Fixed frequency:

	LfNoise2(1000) * 0.05

Modulate frequency:

	LfNoise2(XLn(1000, 10000, 10)) * 0.05

