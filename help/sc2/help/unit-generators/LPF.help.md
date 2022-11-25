# LPF - 2nd order Butterworth lowpass filter

_LPF(in, freq)_

A second order low pass filter.

- in: input signal to be processed
- freq: cutoff frequency.

Modulate frequency:

	LPF(Saw(200) * 0.1, SinOsc(XLn(0.7, 300, 20), 0) * 3600 + 4000)

