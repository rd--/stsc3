;; prepared just piano ; colby leider ; modified by jmcc ; graph rewrite by rd
var c = 4; (* number of combs *)
var a = 4;	(* number of allpasses *)
var v = 3; (* number of overlapped voices *)
OverlapTexture({ :tr |
	var y = Dust(MouseX(0.1, 4, 1, 0.2)) * 0.4;
	y := Resonz(y, 400, 0.4);
	y := Lpf(y, 12000);
	y := Integrator(y, 0.99);
	y := Hpf(y, 200);
	y := {
		CombL(
			y,
			0.1,
			TRand(1, 8, tr) / TRand(1, 9, tr) * 0.001, (* Partch tonality diamond about 1000Hz *)
			5
		)
	}.dup(c).sum;
	a.timesRepeat {
		y := AllpassN(y, 0.040, { TRand(0, 0.040, tr) } ! 2, 8)
	};
	y
}, 5, 3, v)
