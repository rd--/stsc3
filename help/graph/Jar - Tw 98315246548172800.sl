(* https://twitter.com/rukano/status/98315246548172800 ; texture variant *)
{ :tr |
	var h = TrChoose(tr, [100, 800, 3000]);
	{ SinOsc(TrExpRand(tr, 60, h), 0) * 0.1 } !^ 40
}.OverlapTexture(5, 7, 2)
