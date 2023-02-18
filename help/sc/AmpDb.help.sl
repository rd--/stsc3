# AmpDb -- convert linear amplitude to decibels

Inverse of _DbAmp_.

	SinOsc(
		[
			Ln(0, 1, 2).AmpDb,
			Ln(-96, 0.001, 2)
		] * 110 + 110,
		0
	) * 0.1
