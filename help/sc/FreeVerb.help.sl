# FreeVerb -- a reverb

_FreeVerb(in, mix=0.33, room=0.5, damp=0.5)_

- in: input signal
- mix: 0 = dry, 1 = wet
- room: room size (0 .. 1)
- damp: high frequency damping (0 .. 1)

Reverberate decaying impulse shaped cubic oscillator:

```
FreeVerb(
	Decay(
		Impulse([1, 1/3], 0),
		0.25
	) * LfCub([1200, 700], 0) * 0.1,
	MouseX(0, 1, 0, 0.2),
	MouseY(0, 1, 0, 0.2),
	LfNoise2(0.1).Range(0, 1)
)
```
