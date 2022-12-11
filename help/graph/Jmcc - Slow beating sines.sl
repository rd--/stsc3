;; slow beating sines (jmcc) #7 ; graph rewrite
OverlapTexture({ :tr |
	var n = 20;
	var p = OrderedCollection.new;
	var q = OrderedCollection.new;
	var f = { :freqArray |
		freqArray.collect({ :freq |
			SinOsc(freq, TRand(0, 2 * pi, tr))
		}).sum
	};
	n.timesRepeat {
		var freq = TRand(24, 84, tr).MidiCps;
		var d = 5;
		p.add(freq);
		{ p.add(freq + TRand(d.negated, d, tr)) } ! 2;
		{ q.add(freq + TRand(d.negated, d, tr)) } ! 3
	};
	[p, q].collect(f) * 0.1 / n
}, 4, 4, 2)
