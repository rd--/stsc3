(* noise beats ; jmcc *)
var imp = Impulse(8, 0);
var trig = Demand(imp, 0, Dseq(inf, [0.2, 0.1, 0.1, 0.2, 0.1, 0.1, 0.2, 0.1])) * imp;
var amp = Decay2(trig, 0.0004, 0.2);
var x = Resonz({ GrayNoise() } ! 2 * amp, 5200, 0.2);
4.timesRepeat {
	x := AllpassN(x, 0.05, { 0.05.Rand } ! 2, 4)
};
x
