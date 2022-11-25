# LFSaw - sawtooth oscillator

_LFSaw(freq, phase)_

A non-band-limited sawtooth oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz
- iphase: initial phase offset. For efficiency reasons this is a value ranging from 0 to 2.

ConstantFixed frequency:

	LFSaw(500, 1) * 0.1

Used as both Oscillator and LFO:

	LFSaw(LFSaw(4, 0) * 400 + 400, 0) * 0.1

