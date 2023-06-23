# DelayC -- delay line

_DelayC(in, maxdelaytime=0.2, delaytime=0.2)_

Simple delay line. DelayN uses no interpolation, DelayL uses linear interpolation, DelayA uses all pass interpolation.

- in: the input signal.
- maxdelaytime: the maximum delay time in seconds. used to initialize the delay buffer size.
- delaytime: delay time in seconds.

Dust randomly triggers Decay to create an exponential decay envelope for the WhiteNoise input source, input is mixed with delay:

	var z = Decay(Dust(1) * 0.5, 0.3) * WhiteNoise();
	DelayC(z, 0.2, 0.2) + z

