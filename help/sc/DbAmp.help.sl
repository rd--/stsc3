# DbAmp -- convert decibels to linear amplitude

Fixed amplitude:

	SinOsc(440, 0) * -24.DbAmp

Modulate amplitude:

	FSinOsc(800, 0) * Ln(-12, -40, 10).DbAmp
