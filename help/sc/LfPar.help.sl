# LfPar -- parabolic oscillator

A sine-like shape made of two parabolas and the integral of a triangular wave.
It has audible odd harmonics and is non-band-limited.
Output ranges from -1 to +1.

- _LfPar(freq, iphase)_

- freq: frequency in Hertz.
- iphase: initial phase offset. For efficiency reasons this is a value ranging from 0 to 4.

Fixed frequency oscillator:

	LfPar([440, 800], 0) * 0.1

Modulating frequency:

	LfPar(XLn(100, 2000, 10), 0) * 0.1

Amplitude modulation:

	LfPar(XLn(1, 200, 10), 0) * SinOsc(440, 0) * 0.1

Used as both oscillator and lfo:

	LfPar(LfPar(3, 0.3) * 200 + 400, 0) * 0.1

Used as phase modulator (behaves like a triangular modulator in fm):

```
[
	SinOsc(440, LfPar(1, 2) * 8 * pi),
	SinOsc(440 + (LfTri(1, 0) * 8 *pi), 0)
] * 0.1
```
