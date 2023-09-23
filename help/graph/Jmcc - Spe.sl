(* spe (jmcc) *)
var t = Impulse(9, 0);
var s = [0, 3, 2, 7, 8, 32, 16, 18, 0, 12, 24, 32];
var f = (Demand(t, 0, Dseq(inf, s)) + 32).MidiCps;
var e = Decay2(t, 0.05, 1) * 0.1;
var z = Rlpf(LfSaw(f, 0) * e, (LfNoise1(1) * 36 + 110).MidiCps, 0.1);
4.timesRepeat {
	z := AllpassN(z, 0.05, { 0.05.Rand } ! 2, 4)
};
z * 0.25
