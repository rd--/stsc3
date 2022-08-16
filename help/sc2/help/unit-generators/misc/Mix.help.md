# Mix - sum an array of inputs

_Mix([...])_

Sums an array of inputs.

Mix an array of sawtooths:

	Mix([
		LFSaw(200.1, 0),
		LFSaw(500.2, 0),
		LFSaw(1200.3, 0),
		LFSaw(700.4, 0)
	]) * 0.04

The above is the same as this due to multichannel expansion:

	Mix(LFSaw([200.1, 500.2, 1200.3, 700.4], 0)) * 0.04

Sine oscillators:

	Mix([
		FSinOsc(600.2, 0),
		FSinOsc(622.0, 0),
		FSinOsc(641.3, 0),
		FSinOsc(677.7, 0)
	]) * 0.1
