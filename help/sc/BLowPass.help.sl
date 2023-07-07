# BLowPass -- 12db/oct rolloff 2nd order resonant low pass filter

The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

_BLowPass(in, freq, rq)_

- in: input signal to be processed.
- freq: cutoff frequency
- rq: the reciprocal of Q. bandwidth / cutoffFreq

Warning: frequency values close to zero may cause loud audio artifacts.

Mouse control of parameters:

```
BLowPass(
	PinkNoise(),
	MouseX(10, 20000, 1, 0.2),
	MouseY(0, 1, 0, 0.2)
) * 0.25
```
