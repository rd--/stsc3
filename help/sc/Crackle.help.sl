# Crackle -- chaotic noise function

_Crackle(param)_

A noise generator based on a chaotic function.

- param: a parameter of the chaotic function with useful values from just below 1 to just above 2. Towards 2 the sound crackles.

Fixed param:

	Crackle(1.95) * 0.1

Modulate param:

	Crackle(Ln(1, 2, 10)) * 0.1

