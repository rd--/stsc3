# Rhpf -- resonant high pass filter

_Rhpf(in, freq, q)_

A resonant high pass filter.

- in: input signal to be processed
- freq: cutoff frequency.
- rq: the reciprocal of Q. bandwidth / cutoffFreq.

Modulate frequency:

	Rhpf(
		Saw(200) * 0.1,
		FSinOsc(XLn(0.7, 300, 20), 0) * 3600 + 4000,
		0.2
	)

