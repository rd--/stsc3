# LfTri -- triangle wave oscillator

_LfTri(freq=440, phase=0)_

A non-band-limited triangle wave oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz
- iphase: initial phase offset. For efficiency reasons this is a value ranging from 0 to 4.

Constant frequency:

	LfTri(500, 0) * 0.1

Used as both Oscillator and Lfo:

	LfTri(LfTri(1, 0) * 400 + 400, 0) * 0.1

