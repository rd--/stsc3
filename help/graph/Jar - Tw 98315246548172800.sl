(* https://twitter.com/rukano/status/98315246548172800 ; texture variant *)
{ :tr |
	var h = TChoose(tr, [100 800 3000]);
	{ SinOsc(TExpRand(60, h, tr), 0) * 0.1 } !^ 40
}.OverlapTexture(5, 7, 2).Mix
