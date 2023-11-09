(* 20060911 ; rd *)
var x = MouseX(0.03, 0.12, 1, 0.1);
var y = MouseY(0.01, 0.52, 1, 0.1) * 0.4;
var n = {
	var t = [
		SinOsc(Rand(0.3, 0.5), Rand(0, pi)) + 1,
		Impulse(LfNoise2(1 / 3) * 11 + 11, 0),
		0.5
	].product;
	Ringz(
		CoinGate(
			[0.05, LfNoise0(2), y, t].sum.Lpz2,
			t
		),
		TrExpRand(t, [500, 900], 1600),
		x
	)
};
(n !+ 3).Clip2(TrRand(Dust(8), 0, 1)) * 0.25
