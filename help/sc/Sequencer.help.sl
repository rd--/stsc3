# Sequencer -- clocked values

_Sequencer(sequence, trig)_

Outputs a different value from the sequence each time a trigger is received.

- sequence: the values in the array are output cyclically
- trig: trigger when signal changes from non-positive to positive

Sequence triggered by square wave:

```
var freq = Sequencer(
	[60, 62, 63, 58, 48, 55].MidiCps,
	LfPulse(6, 0, 0.5)
);
LfSaw(freq, 0) * 0.1
```

Sequence triggered by random impulses:

```
var freq = Sequencer(
	[60, 62, 63, 58, 48, 55].MidiCps,
	Dust(6)
);
LfSaw(freq, 0) * 0.1
```

Sequence of a demand rate value:

```
var tr = LfPulse(6, 0, 0.5);
var freq = {
	Sequencer(Drand(inf, [1, 2, 3, 7, 8]), tr) * 30 + 340
} ! 2;
SinOsc(freq, 0) * 0.1
```

Sequence of shifting sequences:

```
var clock = Impulse(8, 0);
var root = Sequencer(
	[24, 26, 24, 22],
	PulseDivider(clock, 64, 0)
);
var note = Sequencer(
	[
		33, 33, 35, 36, 45, 47, 38, 40,
		33, 33, 35, 36, 47, 48, 50, 52
	],
	clock
);
var freq = (note + root).MidiCps;
var trig = ImpulseSequencer(
	[0.4, 0.0, 0.1, 0.1, 0.4, 0.1, 0.1, 0.1],
	clock
);
var env = Decay2(trig, 0.005, 1.4) * 0.25;
var z = VarSaw(
	freq * [1, 1.505],
	0,
	MouseY(0, 1, 0, 0.2)
) * env;
CombN(z, 0.26, 0.26, 4).SoftClip
```
