# LFNoise0 - step noise

_LFNoise0(freq)_

Generates random values at a rate given by the nearest integer division of the sample rate by the freq argument.

- freq: approximate rate at which to generate random values.

Fixed frequency:

	LFNoise0(1000) * 0.05

Modulate frequency:

	LFNoise0(XLn(1000, 10000, 10)) * 0.05

