# Median -- median filter

_Median(length, in)_

Returns the median of the last length input points. This non linear filter is good at reducing impulse noise from a signal.

- length: number of input points in which to find the median. Must be an odd number from 1 to 31. If length is 1 then Median has no effect.
- in: input signal to be processed

A signal with impulse noise.

	Saw(500) * 0.1 + (Dust2(100) * 0.9)

After applying median filter:

	var z = Saw(500) * 0.1 + (Dust2(100) * 0.9);
	Median(3, z)

The median length can be increased for longer duration noise.

A signal with longer impulse noise:

	Saw(500) * 0.1 + (LPZ1(Dust2(100) * 0.9))

Length 3 doesn't help here because the impulses are 2 samples long.

	var z = Saw(500) * 0.1 + (LPZ1(Dust2(100) * 0.9));
	Median(3, z)

Length 5 does better:

	var z = Saw(500) * 0.1 + (LPZ1(Dust2(100) * 0.9));
	Median(5, z)

Long Median filters begin chopping off the peaks of the waveform:

	var x = SinOsc(1000, 0) * 0.1;
	XFade2(x, Median(31, x), MouseX(-1, 1, 0, 0.2), 1)

Another noise reduction application:

	WhiteNoise() + SinOsc(800, 0) * 0.1

Use Median filter for high frequency noise:

	var z = WhiteNoise() + SinOsc(800, 0) * 0.1;
	Median(31, z)

Use LeakDc for low frequency noise:

	var z = WhiteNoise() + SinOsc(800, 0) * 0.1;
	LeakDc(Median(31, z), 0.9)

