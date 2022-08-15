# HPF - 2nd order Butterworth highpass filter

_HPF(in, freq)_

A second order high pass filter.

- in: input signal to be processed
- freq: cutoff frequency.

Modulate frequency, note makeup gain:

	HPF(Saw(200) * 0.1, FSinOsc(XLn(0.7, 300, 20), 0) * 3600 + 4000) * 2

