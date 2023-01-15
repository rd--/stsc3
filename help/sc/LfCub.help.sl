# LfCub -- oscillator

_LfCub(freq, iphase)_

A sine like shape made of two cubic pieces. Smoother than _LfPar_.

- freq: frequency in Hertz
- iphase: initial phase offset, range=[0, 2]

As frequency modulator:

	LfCub(LfCub(0.2, 0) * 400 + 800, 0) * 0.1

As nested frequency modulator:

	LfCub(LfCub(LfCub(0.2, 0) * 8 + 10, 0) * 400 + 800, 0) * 0.1

Frequency sweep:

	LfCub(XLn(100, 8000, 30), 0) * 0.1
