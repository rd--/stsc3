# Slope -- slope of signal

_Slope(in)_

Measures the rate of change per second of a signal.  Formula implemented is: _out[i] = (in[i] - in[i-1]) * sampling_rate_.

- in: input signal to measure.

Noise and derivatives control frequency of sine oscillator:

	var a = LfNoise2(2); (* quadratic noise *)
	var b = Slope(a); (* first derivative produces line segments *)
	var c = Slope(b); (* second derivative produces constant segments *)
	var scale = 0.2; (* needed to scale back to +/- 1.0 *)
	Splay2(SinOsc([a, b * scale, c * scale.squared] * 100 + 200, 0)) * 0.1

For another example of Slope see [hypot].
