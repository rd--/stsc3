# Brf -- 2nd order Butterworth band reject filter

_Brf(in, freq, rq)_

A second order low pass filter.

- in: input signal to be processed
- freq: cutoff frequency in Hertz.
- rq: the reciprocal of Q. bandwidth / cutoffFreq.

Modulate frequency:

```
Brf(
	Saw(200) * 0.1,
	FSinOsc(XLn(0.7, 300, 20), 0) * 3800 + 4000,
	0.3
)
```
