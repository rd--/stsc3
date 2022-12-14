# LinPan2 -- two channel linear pan

_LinPan2(in, pos, level)_

Two channel linear panner. This one sounds to me more like the Rhodes tremolo than Pan2.

- in: input signal
- pos: pan position, -1 is left, +1 is right

Pan noise:

	LinPan2(PinkNoise(), FSinOsc(2, 0), 0.1)

Pan sine:

	LinPan2(FSinOsc(800, 0), FSinOsc(3, 0), 0.1)

