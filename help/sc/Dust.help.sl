# Dust -- random impulses

_Dust(density)_

Generates random impulses from 0 to +1.

- density: average number of impulses per second

Fixed density:

	Dust(200) * 0.1

Modulate density:

	Dust(XLn(20000, 2, 10)) * 0.1

