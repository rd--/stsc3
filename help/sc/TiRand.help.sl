# TiRand -- triggered integer random number generator

_TiRand(lo, hi, trig)_

Generates a random integer value in uniform distribution from lo to hi each time the trig signal changes from nonpositive to positive values

	var trig = Dust(10);
	SinOsc(TiRand(4, 12, trig) * 100, 0) * 0.1

Mouse controls density:

	var trig = Dust(MouseX(1, 8000, 1, 0.2));
	SinOsc(TiRand(4, 12, trig) * 100, 0) * 0.1

Random degree, octave and number of harmonics:

	var scale = [0, 2, 4, 5, 7, 9, 10, 12].asLocalBuf;
	{ :tr |
		{
			var degree = TiRand(0, 7, tr);
			var octave = TiRand(4, 7, tr);
			var pitchClass = Index(scale, degree);
			var mnn = (octave * 12) + pitchClass;
			var numHarm = TiRand(1, 4, tr);
			Blip(mnn.MidiCps, numHarm) * 0.05
		} !^ 7
	}.OverlapTexture(4, 0.05, 2)

* * *

See also: IRand, TRand
