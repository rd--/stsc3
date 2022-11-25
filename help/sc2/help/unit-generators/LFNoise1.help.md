# LFNoise1 - ramp noise

_LFNoise1(freq)_

Generates linearly interpolated random values at a rate given by the nearest integer division of the sample rate by the freq argument.

- freq: approximate rate at which to generate random values.

Fixed frequency:

	LFNoise1(1000) * 0.05

Modulate frequency:

	LFNoise1(XLn(1000, 10000, 10)) * 0.05

