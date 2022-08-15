# LFTri - triangle oscillator

_LFTri(freq, iphase)_

A non-band-limited triangle oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz
- iphase: initial phase offset. For efficiency reasons this is a value ranging from 0 to 4.

Constant frequency:

	LFTri(500, 0) * 0.1

Used as both Oscillator and LFO:

	LFTri(LFTri(4, 0) * 200 + 400, 0) * 0.1

