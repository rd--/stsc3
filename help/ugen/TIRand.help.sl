(* IRand *)
var scale = [0 2 4 5 7 9 10 12].asLocalBuf;
{ :tr |
	{
		var degree = IRand(tr, 0, 7);
		var octave = IRand(tr, 4, 7);
		var pitchClass = Index(scale, degree);
		var mnn = (octave * 12) + pitchClass;
		var numHarm = IRand(tr, 1, 4);
		Blip(mnn.MidiCps, numHarm) * 0.1
	} ! 7
}.OverlapTexture(4, 0.05, 2).Mix
