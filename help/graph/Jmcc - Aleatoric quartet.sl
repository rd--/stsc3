(* aleatoric quartet (jmcc) #7 *)
var amp = 0.07;
var density = MouseX(0.01, 1, 0, 0.1);
var dMul = density.Recip * 0.5 * amp;
var dAdd = amp - dMul;
var rapf = { :in |
	AllpassN(in, 0.05, { Rand(0, 0.05) } ! 2, 1)
};
var mkFreq = {
	var n0 = LfNoise0(Select(IRand(0, 2), [1, 0.5, 0.25]));
	Lag((n0 * 7 + 66 + Rand(-30, 30)).RoundTo(1), 0.2).MidiCps
};
var g = (1 .. 4).collect { :ix |
	var x = PinkNoise() * (LfNoise1(8) * dMul + dAdd).Max(0);
	EqPan2(CombL(x, 0.02, mkFreq().Recip, 3), Rand(-1, 1))
}.Sum;
5.timesRepeat {
	g := rapf(g)
};
LeakDc(g, 0.995)
