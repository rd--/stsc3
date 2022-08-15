# LFSaw - sawtooth oscillator

_LFSaw(freq, phase)_

A non-band-limited sawtooth oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz

Fixed frequency:

	LFSaw(500, 0) * 0.1

Used as both Oscillator and LFO:

	LFSaw(LFSaw(4, 0) * 400 + 400, 0) * 0.1

