(* random pulsations (jmcc) #1 ; graph rewrite *)
{ :tr |
	var o1 = SinOsc(TRand(0, 2000, tr), 0);
	var o2 = SinOsc(8 + TRand(0, 80, tr), 0);
	var o3 = SinOsc(0.3 + TRand(0, 0.5, tr), TRand(0, 2.pi, tr)) * 0.7;
	EqPan2(o1.AmClip(o2), o3) * 0.05
}.OverlapTexture(5, 2, 6).Mix
