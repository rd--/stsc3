# LFTri - triangle wave oscillator

_LFTri(freq, phase)_

A non-band-limited triangle wave oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz

Used as both Oscillator and LFO:

	LFTri(LFTri(1, 0) * 400 + 400, 0) * 0.1

