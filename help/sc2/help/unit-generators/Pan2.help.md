# Pan2 - two channel equal power pan

_Pan2(in, pos, level)_

Two channel equal power panner.

- in: input signal
- pos: pan position, -1 is left, +1 is right
- level: a control rate level input.

Pan noise:

	Pan2(PinkNoise(), FSinOsc(2, 0), 0.1)

