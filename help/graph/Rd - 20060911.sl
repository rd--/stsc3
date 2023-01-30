;; 20060911 ; rd
var t = Impulse(22, 0) * (SinOsc(0.5, 0) + 1);
var x = MouseX(0.005, 0.12, 1, 0.1);
var y = MouseY(0.01, 0.52, 1, 0.1);
var n = {
	var n1 = LfNoise0(2);
	var n2 = CoinGate(0.05 + n1 + (y * 0.4) + (t * 0.5), t * 0.5);
	var n3 = TExpRand([500, 900], 1600, t);
	Ringz(n2, n3, x)
};
var b = TRand(0, 1, Dust(8));
(n !+ 3).Clip2(b) * 0.25
