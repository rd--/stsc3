# Rand -- random number generator

- _Rand(lo, hi)_
- _Rand(hi)_ ≡ _Rand(0, hi)_

Generates a single random float value in uniform distribution from lo to hi. It generates this when the SynthDef first starts playing, and remains fixed.

	{
		var freq = Rand(200, 800);
		var dur = (1 / freq) * 7500;
		FSinOsc(freq, 0) * Ln(0.2, 0, 1)
	} !^ 5

* * *

See also: Rand2
