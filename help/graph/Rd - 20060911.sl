(* 20060911 ; rd *)
var x = MouseX(0.03, 0.12, 1, 0.1);
var y = MouseY(0.01, 0.52, 1, 0.1) * 0.4;
var z = {
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
		TExpRand([500 900], 1600, t),
		x
	)
} !> 3;
z.Clip2(TRand(0, 1, Dust(8))) * 0.25
