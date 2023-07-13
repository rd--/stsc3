# Select -- select output from an array of inputs

- _Select(which, array)_

The output is selected from an array of inputs.

All the Ugens are continuously running.
This may not be the most efficient way if each input is Cpu-expensive.
Note that the array is fixed at the time of writing the SynthDef, and the whole array is embedded in the SynthDef file itself.
For small arrays this is more efficient than reading from a buffer.

- which: integer index (zero indexed)
- array: input array of signals

Cycle though oscillator types:

```
var a = [
	SinOsc(440, 0),
	Saw(440),
	Pulse(440, 0.5)
];
var cycle = a.size * 0.5;
Select(LfSaw(1, 0) * cycle + cycle, a) * 0.2
```

As a sequencer:

```
{
	{
		var a = { Rand(30,80) } ! 32;
		var cycle = a.size * 0.5;
		Saw(
			Select(
				LfSaw(1, 0) * cycle + cycle,
				a.MidiCps
			)
		) * 0.2
	} ! 2
}.xfade(3, 4)
```

