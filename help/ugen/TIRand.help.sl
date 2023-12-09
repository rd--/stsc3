(* TiRand *)
var scale = [0 2 4 5 7 9 10 12].asLocalBuf;
{ :tr |
	{
		var degree = TiRand(0, 7, tr);
		var octave = TiRand(4, 7, tr);
		var pitchClass = Index(scale, degree);
		var mnn = (octave * 12) + pitchClass;
		var numHarm = TiRand(1, 4, tr);
		Blip(mnn.MidiCps, numHarm) * 0.1
	} ! 7
}.OverlapTexture(4, 0.05, 2).Mix
