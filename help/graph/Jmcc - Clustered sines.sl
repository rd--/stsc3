(* clustered sines ; jmcc #2 ; graph rewrite *)
var n = 80; (* udp packet limit *)
{ :tr |
	var f1 = TrRand(tr, 100, 1100);
	var f2 = 4.0 * f1;
	var fn = {
		var y = f1 + TrRand(tr, 0, f2);
		SinOsc(y, 0) * f1 / y
	};
	{ fn !+ n } ! 2 * 0.1 / n
}.OverlapTexture(4, 4, 3)
