(* pkt 26 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/490 *)
var n = 8;
(1 .. n).collect { :i |
	var t = i / n;
	var a = VarSaw(0.02, t, 0.5) * 7.5;
	var b = VarSaw(0.16, t, 2/3) * a.Abs + 300;
	var c = VarSaw(0.064, t, 0.5) * 25 + 50;
	var d = VarSaw(0.012, t, 0.75) * c + 200;
	var e = VarSaw(0.024, t, 0.25) * 0.475 + 0.5;
	var f = VarSaw(100 + i, t, e);
	var g = VarSaw(0.048, 0, 0.5) * 25 + 150;
	var h = VarSaw(i + 1 * g, t, 1/3) * 150;
	var o = SinOsc(h, f * pi) * d + b;
	var z = LeakDc(VarSaw(o, t, 0.5), 0.995);
	EqPan2(z, VarSaw(0.02, t, 0.5)) / n
}.Mix * 0.35
