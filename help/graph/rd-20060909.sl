;; 20060909 ; rd
var x = MouseX(0.001, 0.02, 1, 0.1);
var y = MouseY(120, 400, 1, 0.1);
var p = {
	var f = LfNoise0(4) * [32, 64];
	var w = LfNoise0(32) * x;
	var z = LfNoise0(2) * 0.1;
	var m = LfNoise0(6);
	var s = Pulse(f, w);
	Resonz(s, y + z, (m * 0.4) + 0.8) * 0.5
};
var q = {
	var n = LfNoise0(128);
	CombN(p(), 0.2, (n * 0.1) + 0.1, 3)
};
var r = {
	var x1 = MouseX(0.75, 1.25, 1, 0.1);
	var y1 = MouseY(0.25, 1, 1, 0.1);
	var f = {
		var fr = Rand(50, 59) * x1;
		var am = Rand(0.04, 0.16) * y1;
		SinOsc(fr, 0) * am
	};
	{ f !+ 16 } ! 2
};
p() + q() + r()
