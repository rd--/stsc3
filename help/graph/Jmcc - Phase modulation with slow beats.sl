(* phase modulation with slow beats ; jmcc #6 ; graph rewrite *)
var n = 4;
var x = MouseX(100, 6000, 1, 0.2); (* controls random frequency of new events *)
var y = MouseY(0, 2, 0, 0.2); (* controls modulation index *)
{ :tr |
	var a = 0;
	var f1 = TRand(0, x, tr);
	3.timesRepeat {
		var f2 = TRand(0, x, tr);
		a := SinOsc([f2, f2 + TRand(-1, 1, tr)], 0) * y + a
	};
	SinOsc([f1, f1 + TRand(-1, 1, tr)], a) * 0.1
}.OverlapTexture(4, 4, n).Mix * 0.25
