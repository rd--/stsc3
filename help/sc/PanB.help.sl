# PanB -- Ambisonic B format panner

PanB(in, azimuth, elevation, level)

- in: input signal
- azimuth: in radians, -pi to +pi
- elevation: in radians, -0.5pi to +0.5pi
- level: a control rate level input.

Output channels are in order W,X,Y,Z. You will only hear the first two channels on a stereo setup.

	PanB(
		WhiteNoise(),
		LfSaw(0.5, 0) * pi,
		FSinOsc(0.31, 0) * 0.5 * pi,
		0.1
	)
