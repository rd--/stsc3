# ImpulseSequencer -- clocked impulses

_ImpulseSequencer(sequence, trig)_

Outputs a single sample impulse level from the sequence each time a trigger is received.

- sequence: the values in the array are output cyclically
- trig: trigger occurs when signal changes from non-positive to positive

Use a pulse wave as a trigger:

```
var seq = { :index |
	var p = [
		[
			1, 0, 0, 1, 0, 0, 1, 0,
			0, 0, 1, 0, 1, 0, 0, 0
		],
		[
			0, 0, 0, 0, 0, 0, 0, 0,
			0, 0, 0, 0, 0, 0, 0, 0,
			1, 1, 1, 1, 1, 0, 0, 0
		],
		[
			1, 0.1, 0.1, 1, 0.1, 1, 0.1, 0.1
		],
		[
			1, 0, 0.2, 0, 0.2, 0, 0.2, 0
		]
	];
	var dt = [
		0.3, 0.3, 0.25, 0.5
	];
	Decay2(
		ImpulseSequencer(p[index], t),
		0.001,
		dt[index]
	)
};
var t = LfPulse(8, 0, 0.5);
[
	1.seq * SinOsc(1700, 0) * 0.1,
	2.seq * SinOsc(2400, 0) * 0.04,
	3.seq * BrownNoise() * 0.05,
	4.seq * SinOsc(100, 0) * 0.2
].sum
```
