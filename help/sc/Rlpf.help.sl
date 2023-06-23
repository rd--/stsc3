# Rlpf -- resonant low pass filter

_Rlpf(in, freq=440, rq=1)_

A resonant low pass filter.

- in: input signal to be processed
- freq: cutoff frequency
- rq: the reciprocal of Q, bandwidth / cutoffFreq

Modulate frequency:

	Rlpf(
		Saw(200) * 0.1,
		FSinOsc(XLn(0.7, 300, 20), 0) * 3600 + 4000,
		0.2
	)
