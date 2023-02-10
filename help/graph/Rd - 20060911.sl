;; 20060911 ; rd
var n = {
	var t = SinOsc(Rand(0.3, 0.5), Rand(0, pi)) + 1 * Impulse(LfNoise2(1 / 3) * 11 + 11, 0);
	Ringz(
		CoinGate([
			0.05,
			LfNoise0(2),
			MouseY(0.01, 0.52, 1, 0.1) * 0.4,
			t * 0.5
		].sum.Lpz2, t * 0.5),
		TExpRand([500, 900], 1600, t),
		MouseX(0.03, 0.12, 1, 0.1)
	)
};
(n !+ 3).Clip2(TRand(0, 1, Dust(8))) * 0.25
