(* deep trip (jmcc) #9 ; graph rewrite *)
{ :tr |
	var f = (LfNoise1(TrRand(tr, 0, 0.3)) * 60 + 70).MidiCps;
	var a = LfNoise2(f * TrRand(tr, 0, 0.5)) * (LfNoise1(TrRand(tr, 0, 8)) * SinOsc(TrRand(tr, 0, 40), 0) * 0.1).Max(0);
	var z = SinOsc(f, 0) * a;
	var s = Pan2(z, LfNoise1(TrRand(tr, 0, 5)), 1);
	var c = {
		CombN(s, 0.5, { TrRand(tr, 0, 0.2) + 0.3 } ! 2, 20)
	};
	c !+ 2 + s
}.OverlapTexture(12, 4, 4)
