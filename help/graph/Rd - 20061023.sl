;; 20061023 ; rd
var n1 = LfNoise0([0.5, 1.5]);
var o = SinOsc(n1, 0);
var a = Slope(o).Abs * [2, 3];
var t = Impulse(a, 0);
var i = Decay2(t, 0.1, 0.2);
var x = MouseX(960, 3620, 1, 0.2);
var y = MouseY(0.5, 2.0, 0, 0.2);
var n2 = TRand(x, 3940, t);
var n3 = TRand(0.005, 0.275, t);
Ringz(i, n2, n3 * y)
