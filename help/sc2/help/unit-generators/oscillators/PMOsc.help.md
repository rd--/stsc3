# PMOsc - phase modulation oscillator pair

_PMOsc(carfreq, modfreq, index, modphase)_

Phase modulation sine oscillator pair.

- carfreq: carrier frequency in cycles per second.
- modfreq: modulator frequency in cycles per second.
- index: modulation index in radians.
- modphase: a modulation input for the modulator's phase in radians

Modulate carfreq:

	PMOsc(Ln(600, 900, 5), 600, 3, 0) * 0.1

Modulate modfreq:

	PMOsc(300, Ln(600, 900, 5), 3, 0) * 0.1

Modulate index:

	PMOsc(300, 550, Ln(0, 20, 8), 0) * 0.1

Texture:

	OverlapTexture({
		arg tr;
		LinPan2(PMOsc(TRand(20, 2000, tr), TRand(0, 800, tr), TLine(0, TRand(0, 12, tr), 9), 0), TRand(-1, 1, tr), 0.1)
	}, 2, 5, 4)

