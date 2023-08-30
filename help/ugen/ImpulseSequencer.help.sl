(* ImpulseSequencer ; jmcc *)
var trig = ImpulseSequencer([0.2, 0.1, 0.1, 0.2, 0.1, 0.1, 0.2, 0.1], Impulse(8, 0));
var amp = Decay2(trig, 0.0004, 0.2);
var x = Resonz({ GrayNoise() } ! 2 * amp, 5200, 0.2);
4.timesRepeat {
	x := AllpassN(x, 0.05, { 0.05.Rand } ! 2, 4)
};
x

