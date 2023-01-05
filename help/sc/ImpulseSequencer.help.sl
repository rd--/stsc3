# ImpulseSequencer -- clocked impulses

_ImpulseSequencer(sequence, trig)_

Outputs a single sample impulse level from the sequence each time a trigger is received.

- sequence: the values in the array are output cyclically
- trig: trigger occurs when signal changes from non-positive to positive

Use a pulse wave as a trigger:

```
var t = LfPulse(8, 0, 0.5);
[
	Decay2(
		ImpulseSequencer([1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0], t),
		0.001,
		0.3
	) * SinOsc(1700, 0) * 0.1,
	Decay2(
		ImpulseSequencer([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0], t),
		0.001,
		0.3
	) * SinOsc(2400, 0) * 0.04,
	Decay2(
		ImpulseSequencer([1.0, 0.1, 0.1, 1.0, 0.1, 1.0, 0.1, 0.1], t),
		0.001,
		0.25
	) * BrownNoise() * 0.05,
	Decay2(
		ImpulseSequencer([1, 0, 0.2, 0, 0.2, 0, 0.2, 0], t),
		0.001,
		0.5
	) * SinOsc(100, 0) * 0.2
].sum
```
