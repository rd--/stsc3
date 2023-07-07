# MultiTapDelay -- multi-tap delay line

_MultiTapDelay(timesArray, levelsArray, in)_

Multi tap delay line. The last delay time is the cycle length, and will coincide with the input signal.

- timesArray: an array of delay times in seconds.
- levelsArray: an array of amplitudes.
- in: the input signal.

Two unequally spaced echoes:

	var z = Decay2(Impulse(1 / 2, 0), 0.01, 0.1) * PinkNoise();
	[z, MultiTapDelay([0.5, 1.25, 1.5], [0.1, 1, 0], z)]

Four equally spaced echoes, each louder than the last:

	var z = Decay2(Dust(2), 0.01, 0.1) * PinkNoise();
	[z, MultiTapDelay([0.1, 0.2, 0.3, 0.4, 1], [0.1, 0.2, 0.8, 1, 0], z)]

Four second cyclic pattern of four repeats:

	var z = Decay2(Impulse(1 / 4, 0), 0.01, 0.1) * PinkNoise();
	[z, MultiTapDelay([0.5, 1, 1.9, 2, 4], [0.1, 1, 0.5, 0.2, 0], z)]

