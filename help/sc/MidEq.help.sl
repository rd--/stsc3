# MidEq -- parametric filter

Attenuates or boosts a frequency band.

_MidEq(in, freq=440, rq=1, db=0)_

- in: the input signal
- freq: center frequency of the band in hertz
- rq: the reciprocal of Q (bandwidth / cutoffFreq)
- db: amount of boost (db > 0) or attenuation (db < 0) of the frequency band

Mixer parametric Eq as effect:

```
MidEq(
	Saw(200) * 0.1,
	SinOsc(LfNoise2(1 / 3), 0).MulAdd(24, 84).MidiCps,
	0.3,
	12
)
```

Notch filter:

```
var in = PinkNoise() + SinOsc(600, 0) * 0.1;
MidEq(in, SinOsc(0.2, pi / 2) * 2 + 600, 0.01, -24)
```
