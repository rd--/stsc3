# Ringz -- ringing filter

_Ringz(in, freq, decaytime)_

This is the same as Resonz, except that instead of a resonance parameter, the bandwidth is specified in a 60dB ring decay time. One Ringz is equivalent to one component of the Klank UGen.

- in: input signal to be processed
- freq: resonant frequency in Hertz
- decaytime: the 60 dB decay time of the filter

Resonant dust:

	Ringz(Dust(3) * 0.1, 2000, 2)

Resonant noise:

	Ringz(WhiteNoise() * 0.005, 2000, 0.5)

Modulate frequency:

	var freq = XLn(100, 3000, 10);
	[
		Ringz(WhiteNoise() * 0.005, freq, 0.5),
		Ringz(Impulse(6, 0) * 0.1, freq, 0.5)
	]

Modulate ring time:

	Ringz(Impulse(6, 0) * 0.1, 2000, XLn(4, 0.04, 8))

Modulate ring time opposite direction:

	Ringz(Impulse(6, 0) * 0.1, 2000, XLn(0.04, 4, 8))

Parallel filters with frequency ramps:

	var exciter = WhiteNoise() * 0.001;
	{
		Ringz(
			exciter,
			XLn(
				ExpRand(100, 5000),
				ExpRand(100, 5000),
				20
			),
			0.5
		)
	} !^ 10

Texture of above:

	{ :tr |
		var exciter = WhiteNoise() * 0.001;
		{
			Ringz(
				exciter,
				TxLine(
					TExpRand(100, 5000, tr),
					TExpRand(100, 5000, tr),
					20,
					tr
				),
				0.5
			)
		} !^ 10
	}.OverlapTexture(5, 10, 3)

