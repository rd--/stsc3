(* coolant ; jmcc #2 ; graph rewrite ; requires=XFadeTexture *)
var o = OnePole(BrownNoise() * 0.002, 0.95);
XFadeTexture({ :tr |
	{
		Ringz(o, 40 + TRand(0, 2000, tr), 1) * 0.5
	} !^ 10
}, 4, 4)
