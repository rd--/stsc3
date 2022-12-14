# Mix -- sum an array of inputs

_Mix([...])_

Sums an array of inputs.

Mix an array of sawtooths:

	[
		LfSaw(200.1, 0),
		LfSaw(500.2, 0),
		LfSaw(1200.3, 0),
		LfSaw(700.4, 0)
	].Mix * 0.04

The above is the same as this due to multichannel expansion:

	LfSaw([200.1, 500.2, 1200.3, 700.4], 0).Mix * 0.04

Sine oscillators:

	[
		FSinOsc(600.2, 0),
		FSinOsc(622.0, 0),
		FSinOsc(641.3, 0),
		FSinOsc(677.7, 0)
	].Mix * 0.1
