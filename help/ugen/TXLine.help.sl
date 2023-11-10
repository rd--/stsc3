(* XLine *)
var tr = Impulse(0.25, 0);
var gainEnv = Decay2(tr, 0.01, 1);
EqPan2(
	Saw(XLine(tr, 100, 1000, 0.1)),
	Line(tr, -1, 1, 0.75)
) * gainEnv
