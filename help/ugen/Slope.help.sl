(* Slope ; frequency is the slope of phase (scaled) *)
var freq = 440;
var rate = 2 * pi * freq / SampleRate();
var phase = Phasor(1, rate, 0, 2 * pi, 0);
[
	SinOsc(freq, 0),
	SinOsc(Slope(phase / pi / 2), 0)
] * 0.1
