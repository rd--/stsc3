# GrainFm -- granular synthesis with frequency modulated sine tones

_GrainFm(numChannels, trigger, dur, carfreq, modfreq, index, pan, envbufnum, maxGrains)_

- numChannels: the number of channels to output
- trigger: a trigger to start a new grain
- dur: size of the grain (in seconds)
- carfreq: the carrier freq of the grain generators internal oscillator
- modfreq: the modulating freq of the grain generators internal oscillator
- index: the index of modulation
- pan: determines where to pan the output
- envbufnum: the buffer number containing a signal to use for the grain envelope, -1 uses a built-in Hann envelope.
- maxGrains: the maximum number of overlapping grains that can be used at a given time (ir)

Mouse controls panning, noise and mouse control deviation from center pitch:

```
var tr = Impulse(10, 0);
GrainFm(
	2,
	tr,
	0.1,
	440 + WhiteNoise() * MouseY(0, 400, 0, 0.2),
	TRand(20, 200, tr),
	LfNoise1(500).Range(1, 10),
	MouseX(-1, 1, 0, 0.2),
	-1,
	512
) * 0.1
```
