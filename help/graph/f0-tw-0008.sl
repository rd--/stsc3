;; https://sccode.org/1-4Qy ; tweet0008
var x = LfNoise1([0.5, 0.5]);
var o = Crackle(LinLin(x, -1, 1, 1.8, 1.98));
Formlet(o, Lag(TExpRand(200, 2000, x), 2), LinLin(x, -1, 1, 0.0005, 0.001), 0.0012) * 0.25
