# OctCps -- convert decimal octaves to cycles per second

Inverse of _CpsOct_.

	Saw(Ln(2, 9, 10).OctCps) * 0.1

Decimal octaves are a linear pitch space.
_C4_ is _4_, _A4_ is _4 + (9/12)_.

	(4 + (9 / 12)).OctCps = 440
