# LfNoise0 -- step noise

_LfNoise0(freq)_

Generates random values at a rate given by the nearest integer division of the sample rate by the freq argument.

- freq: approximate rate at which to generate random values.

Fixed frequency:

	LfNoise0(1000) * 0.05

Modulate frequency:

	LfNoise0(XLn(1000, 10000, 10)) * 0.05

