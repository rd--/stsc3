# LFNoise2 - quadratic noise

_LFNoise2(freq)_

Generates quadratically interpolated random values at a rate given by the nearest integer division of the sample rate by the freq argument.

- freq: approximate rate at which to generate random values.

Fixed frequency:

	LFNoise2(1000) * 0.05

Modulate frequency:

	LFNoise2(XLn(1000, 10000, 10)) * 0.05

