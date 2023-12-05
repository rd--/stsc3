(* a bath (jrhb) ; requires=SelectX *)
var above = MouseY(0, 2, 0, 0.2).RoundTo(1);
var aside = MouseX(1, 1.6, 0, 0.2);
var sources = [
	{ WhiteNoise() } ! 2 * 0.3,
	{ PinkNoise() } ! 2,
	LfdNoise3(10000) * 0.1
];
var u1 = SelectX(LfdNoise1(4).Range(0, sources.size), sources) * 0.1;
var u2 = u1 + DelayN(u1, 0.1, [0.001, 0.0012]) * 0.1;
var u3 = { Rlpf(u2, { Rand(100.0, 340) * aside } ! 4, 0.2).Sum } ! 2;
var u4 = CombL(u3.reversed, 0.05, 0.05, 0.3) * 0.3 * LfNoise2(0.2).Max(0) + u3;
SelectX(Lag(above, 0.4), [u2, u4]) * 2
