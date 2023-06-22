# Latch -- sample and hold

_Latch(in=0, trig=0)_

Holds input signal value when triggered.

- in: input signal.
- trig: trigger. Trigger can be any signal. A trigger happens when the signal changes from non-positive to positive.

Step noise:

	Latch(WhiteNoise(), Impulse(900, 0)) * 0.1

Step noise as frequency input:

	Blip(Latch(WhiteNoise(), Impulse(9, 0)) * 400 + 500, 4) * 0.1

The above are just meant as examples. _LfNoise0_ is a faster way to generate random steps:

	Blip(LfNoise0(9) * 400 + 500, 4) * 0.1

