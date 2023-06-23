# TRand -- triggered random number generator

_TRand(lo=0, hi=1, trig=0)_

Generates a random float value in uniform distribution from lo to hi each time the trig signal changes from nonpositive to positive values

	var trig = Dust(10);
	SinOsc(TRand(300, 3000, trig), 0) * 0.1

Mouse controls density:

	var trig = Dust(MouseX(1, 8000, 1, 0.2));
	SinOsc(TRand(300, 3000, trig), 0) * 0.1

* * *

See also: _Rand_, _TiRand_

