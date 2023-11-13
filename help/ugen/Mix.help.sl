(* mixByNamedRule *)
{ :tr |
	var ping = SinOsc(2222, 0) * Decay2(tr, 0.01, 0.2);
	var sine = SinOsc(Rand(tr, 220, 330), 0);
	EqPan2(ping + sine, -1) * 0.1
}.OverlapTexture(8, 0, 8).Mix
