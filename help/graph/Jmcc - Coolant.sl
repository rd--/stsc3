(* coolant ; jmcc #2 ; graph rewrite ; requires=XFadeTexture *)
var o = OnePole(BrownNoise() * 0.002, 0.95);
XFadeTexture({ :tr |
	{
		Ringz(o, 40 + Rand(tr, 0, 2000), 1) * 0.5
	} !^ 10
}, 4, 4)
