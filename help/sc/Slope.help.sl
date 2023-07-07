# Slope -- slope of signal

_Slope(in)_

Measures the rate of change per second of a signal. Formula implemented is: _out[i] = (in[i] - in[i-1]) * sampling_rate_.

- in: input signal to measure.

Noise and derivatives control frequency of sine oscillator:

 	(* quadratic noise *)
	var a = LfNoise2(2);
 	(* first derivative produces line segments *)
	var b = Slope(a);
 	(* second derivative produces constant segments *)
	var c = Slope(b);
 	(* needed to scale back to +/- 1.0 *)
	var scale = 0.2;
	var freq = [a, b * scale, c * scale.squared] * 100 + 200;
	Splay2(SinOsc(freq, 0)) * 0.1

For another example of Slope see [hypot].
