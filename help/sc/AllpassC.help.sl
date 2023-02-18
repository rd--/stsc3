# AllpassC -- all pass filter

_AllpassC(in, maxdelaytime, delaytime, decaytime)_

All pass delay line. AllpassN uses no interpolation, AllpassL uses linear interpolation, AllpassC uses cubic interpolation.

- in: the input signal.
- maxdelaytime: the maximum delay time in seconds. used to initialize the delay buffer size.
- delaytime: delay time in seconds.
- decaytime: time for the echoes to decay by 60 decibels. If this time is negative then the feedback coefficient will be negative, thus emphasizing only odd harmonics at an octave lower.

Since the allpass delay has no audible effect as a resonator on steady state sound...

	var z = WhiteNoise() * 0.1;
	AllpassC(z, 0.01, XLn(0.0001, 0.01, 20), 0.2)

...these examples add the input to the effected sound so that you can hear the effect of the phase comb:

	var z = WhiteNoise() * 0.1;
	z + AllpassC(z, 0.01, XLn(0.0001, 0.01, 20), 0.2)

The interpolation schemes result in different signals.

Used as an echo this does not really sound different than _Comb_, but it outputs the input signal immediately (inverted) and the echoes are lower in amplitude.

	AllpassC(
		Decay(Dust(1) * 0.5, 0.2) * WhiteNoise(),
		0.2,
		0.2,
		3
	)

