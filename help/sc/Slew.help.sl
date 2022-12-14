# Slew -- slew rate limiter

_Slew(in, upSlope, downSlope)_

Limits the slope of an input signal. The slope is expressed in units per second.

- in: input signal.
- upSlope: maximum upward slope.
- downSlope: maximum downward slope.

Slew square wave:

	var z = LfPulse(800, 0, 0.5) * 0.1;
	[z, Slew(z, 4000, 4000)]

