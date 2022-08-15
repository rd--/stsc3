# Limiter - peak limiter

_Limiter(input, level, lookAheadTime)_

Limits the input amplitude to the given level. Limiter will not overshoot like _Compander_ will, but it needs to look ahead in the audio. Thus there is a delay equal to twice the lookAheadTime.

Limiter, unlike Compander, is completely transparent for an in range signal.

- input: the signal to be processed.
- level: the peak output amplitude level to which to normalize the input.
- lookAheadTime: the buffer delay time. Shorter times will produce smaller delays and quicker transient response times, but may introduce amplitude modulation artifacts.

	var z = Decay2(Impulse(8, 0) * (LFSaw(0.25, 0) * -0.6 + 0.7), 0.001, 0.3) * FSinOsc(500, 0);
	[z, Limiter(z, 0.4, 0.01)] * 0.2

