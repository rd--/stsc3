(* why supercollider (jmcc) #0 *)
var s = {
	Resonz(
		Dust(0.2) * 50,
		200 + 3000.Rand,
		0.003
	)
} !+ 10;
var x = {
	CombL(
		DelayN(s, 0.048, 0.048),
		0.1,
		LfNoise1(0.1.Rand) * 0.04 + 0.05,
		15
	)
} !+ 7;
4.timesRepeat {
	x := AllpassN(x, 0.050, { 0.050.Rand } ! 2, 1)
};
x * 0.2 + s
