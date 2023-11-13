(* phase modulation with slow beats ; jmcc #6 ; graph rewrite *)
var n = 4;
var x = MouseX(100, 6000, 1, 0.2); (* controls random frequency of new events *)
var y = MouseY(0, 2, 0, 0.2); (* controls modulation index *)
{ :tr |
	var a = 0;
	var f1 = Rand(tr, 0, x);
	3.timesRepeat {
		var f2 = Rand(tr, 0, x);
		a := SinOsc([f2, f2 + Rand(tr, -1, 1)], 0) * y + a
	};
	SinOsc([f1, f1 + Rand(tr, -1, 1)], a) * 0.1
}.OverlapTexture(4, 4, n).Mix * 0.25
