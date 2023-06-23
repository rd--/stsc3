# LfSaw -- sawtooth oscillator

_LfSaw(freq=440, phase=0)_

A non-band-limited sawtooth oscillator. Output ranges from -1 to +1.

- freq: frequency in Hertz
- iphase: initial phase offset. For efficiency reasons this is a value ranging from 0 to 2.

Fixed frequency:

	LfSaw(500, 1) * 0.1

Used as both Oscillator and Lfo:

	LfSaw(LfSaw(4, 0) * 400 + 400, 0) * 0.1

