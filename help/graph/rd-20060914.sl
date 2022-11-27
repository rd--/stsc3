;; 20060914 ; rd ; graph rewrite ; requires=Sine ; requires=arrayedEnv
OverlapTexture({ :tr |
	var chrd = { :m |
		var ds = 3;
		var du = [5, 4, 5, 7, 4, 5];
		var d = du * ds;
		var f = TXLine(m, m + TRand(0.05, 0.5, tr), d, tr).midiCps;
		var e = Sine(tr, du.maxItem * ds) * TRand(0.005, 0.01, tr);
		var p = TXLine(TRand(-1, 1, tr), TRand(-1, 1, tr), d, tr);
		var o = SinOsc(f, 0);
		Pan2(o * e, p, 1).sum
	};
	var scale = [0, 2, 4, 5, 7, 9, 11];
	var octaves = [4, 5, 6, 7];
	var mnn = scale.collect({ :n | octaves.collect({ :o | n + (o * 12) }) }).concatenation;
	var chd = { TChoose(tr, mnn) } ! 6;
	{ chrd(chd) }.dup(9).sum
}, 21, 0, 3)
