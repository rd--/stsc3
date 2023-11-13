(* random panning sines (jmcc) #4 *)
{ :tr |
	var n = 8;
	{
		var osc = SinOsc(80 + Rand(tr, 0, 2000), 0);
		var pos = LfNoise1(0.4 + Rand(tr, 0, 0.8));
		var amp = LfNoise1(0.4 + Rand(tr, 0, 0.8)) * 0.4 + 0.5;
		EqPan2(osc, pos) * amp
	} !+ n * 0.1 / n
}.OverlapTexture(8, 8, 2).Mix
