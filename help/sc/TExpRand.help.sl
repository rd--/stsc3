# TExpRand -- triggered exponential random number generator

_TExpRand(lo, hi, trig)_

Generates a random float value in exponential distribution from lo to hi each time the trig signal changes from nonpositive to positive values lo and hi must both have the same sign and be non-zero.

	var trig = Dust(10);
	SinOsc(TExpRand(300, 3000, trig), 0) * 0.1

Mouse controls density:

	var trig = Dust(MouseX(1, 8000, 1, 0.2));
	SinOsc(TExpRand(300, 3000, trig), 0) * 0.1

* * *

See also: _ExpRand_
