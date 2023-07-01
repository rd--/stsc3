# EqPan2 -- two channel equal power pan

_EqPan2(in, pos=0)_

Two channel equal power panner.

- in: input signal
- pos: pan position, -1 is left, +1 is right

Pan noise:

	EqPan2(PinkNoise(), SinOsc(2, 0)) * 0.1

* * *

See also: _LinPan2_, _Pan2_
