# PmOsc -- phase modulation oscillator pair

_PmOsc(carfreq, modfreq, index=0, modphase=0)_

Phase modulation sine oscillator pair.

- carfreq: carrier frequency in cycles per second.
- modfreq: modulator frequency in cycles per second.
- index: modulation index in radians.
- modphase: a modulation input for the modulators phase in radians

Modulate carfreq:

	PmOsc(Ln(600, 900, 5), 600, 3, 0) * 0.1

Modulate modfreq:

	PmOsc(300, Ln(600, 900, 5), 3, 0) * 0.1

Modulate index:

	PmOsc(300, 550, Ln(0, 20, 8), 0) * 0.1

Texture:

	{ :tr |
		LinPan2(
			PmOsc(
				TRand(20, 2000, tr),
				TRand(0, 800, tr),
				TLine(0, TRand(0, 12, tr), 9, tr),
				0
			),
			TRand(-1, 1, tr),
			0.1
		)
	}.OverlapTexture(2, 5, 4)
