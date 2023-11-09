(* 20060914 ; rd ; graph rewrite ; requires=Sine ; requires=arrayedEnv *)
{ :tr |
	var chrd = { :m |
		var ds = 3;
		var du = [5 4 5 7 4 5];
		var d = du * ds;
		EqPan2(
			SinOsc(TrXLine(tr, m, m + TrRand(tr, 0.05, 0.5), d).MidiCps, 0),
			TrXLine(tr, TrRand(tr, -1, 1), TrRand(tr, -1, 1), d)
		).sum * Sine(tr, du.max * ds) * TrRand(tr, 0.005, 0.01)
	};
	var scale = [0 2 4 5 7 9 11];
	var octaves = [4 5 6 7];
	var mnn = scale.collect { :n |
		octaves.collect { :o |
			n + (o * 12)
		}
	}.concatenation;
	var chd = {
		TrChoose(tr, mnn)
	} ! 6;
	{
		chrd(chd)
	} !+ 7
}.OverlapTexture(21, 0, 3)
