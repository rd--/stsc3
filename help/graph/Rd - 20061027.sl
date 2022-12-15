;; 20061027 ; rd
var h0 = {
	var n = LfNoise0(1) * 5 + 5;
	var a = LfNoise0(1) * 0.2 + 1.2;
	var b = LfNoise0(1) * 0.15 + 0.15;
	var f = 40;
	var h = HenonN([f, f * 0.5], a, b, 0, 0);
	Saw(h * 3200 + 1600) * 0.35
 };
var h1 = {
	var n0 = LfNoise0(32);
	var n1 = LfNoise0(2);
	var a = MouseX(1.2, 1.4, 0, 0.1);
	var b = MouseY(0.2, 0.3, 0, 0.1);
	var h = n0.Range(1, 32);
	var p = n1.Range(2400, 3200);
	var l = n1.Range(-0.75, 0.75);
	var g = n1.Range(0.55, 0.85);
	var f = 40;
	var o = Blip(HenonN(f, a, b, 0, 0).Range(p, p * 2), h);
	EqPan2(o, l) * g * 0.35
 };
h0() + h1()
