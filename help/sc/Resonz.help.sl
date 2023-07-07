# Resonz -- resonant filter

_Resonz(in, freq, rq)_

A two pole resonant filter with zeroes at z = +/- 1. Based on K. Steiglitz, "A Note on Constant-Gain Digital Resonators," Computer Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994.

- in: input signal to be processed
- freq: resonant frequency in Hertz
- rq: bandwidth ratio (reciprocal of Q). rq = bandwidth / centerFreq

The reciprocal of Q is used rather than Q because it saves a divide operation inside the unit generator.

Fixed frequency:

	Resonz(WhiteNoise() * 0.5, 2000, 0.1)

Modulate frequency:

	Resonz(WhiteNoise() * 0.5, XLn(1000, 8000, 10), 0.05)

Modulate bandwidth:

	Resonz(WhiteNoise() * 0.5, 2000, XLn(1, 0.001, 8))

Modulate bandwidth opposite direction:

	Resonz(WhiteNoise() * 0.5, 2000, XLn(0.001, 1, 8))

