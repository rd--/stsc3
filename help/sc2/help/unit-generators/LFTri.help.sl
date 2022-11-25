# LFTri - triangle wave oscillator

_LFTri(freq, phase)_

A non-band-limited triangle wave oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz
- iphase: initial phase offset. For efficiency reasons this is a value ranging from 0 to 4.

Constant frequency:

	LFTri(500, 0) * 0.1

Used as both Oscillator and LFO:

	LFTri(LFTri(1, 0) * 400 + 400, 0) * 0.1

