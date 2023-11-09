(* random pulsations (jmcc) #1 ; graph rewrite *)
{ :tr |
	var o1 = SinOsc(TrRand(tr, 0, 2000), 0);
	var o2 = SinOsc(8 + TrRand(tr, 0, 80), 0);
	var o3 = SinOsc(0.3 + TrRand(tr, 0, 0.5), TrRand(tr, 0, 2 * pi)) * 0.7;
	EqPan2(o1.AmClip(o2), o3) * 0.05
}.OverlapTexture(5, 2, 6)
