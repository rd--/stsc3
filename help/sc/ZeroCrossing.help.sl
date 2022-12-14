# ZeroCrossing -- zero crossing frequency follower

_ZeroCrossing(in)_

Outputs a frequency based upon the distance between interceptions of the X axis. The X intercepts are determined via linear interpolation so this gives better than just integer wavelength resolution. This is a very crude pitch follower, but can be useful in some situations.

- in: input signal.

Track frequency of sine oscillator:

	var a = SinOsc(SinOsc(1, 0) * 600 + 700, 0) * 0.1;
	[a, ZeroCrossing(a) * 0.0005]
