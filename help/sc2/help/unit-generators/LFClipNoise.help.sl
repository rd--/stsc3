# LFClipNoise - clipped noise

_LFClipNoise(freq)_

Randomly generates the values -1 or +1 at a rate given by the nearest integer division of the sample rate by the freq argument. It is probably pretty hard on your speakers!

- freq: approximate rate at which to generate random values.

Fixed frequency:

	LFClipNoise(1000) * 0.05

Modulate frequency:

	LFClipNoise(XLn(1000, 10000, 10)) * 0.05
