# Bpf -- 2nd order Butterworth bandpass filter

_Bpf(in, freq, rq)_

A second order low pass filter.

- in: input signal to be processed
- freq: cutoff frequency in Hertz.
- rq: the reciprocal of Q. bandwidth / cutoffFreq.

Modulate frequency:

```
Bpf(
	Saw(200) * 0.5,
	FSinOsc(XLn(0.7, 300, 20), 0) * 3600 + 4000,
	0.3
)
```
