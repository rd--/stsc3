(* slow beating sines (jmcc) #7 ; graph rewrite ; requires Array>>add *)
{ :tr |
	var n = 20;
	var p = [];
	var q = [];
	var f = { :freqArray |
		freqArray.collect { :freq |
			SinOsc(freq, Rand(tr, 0, 2 * pi))
		}.sum
	};
	n.timesRepeat {
		var freq = Rand(tr, 24, 84).MidiCps;
		var d = 5;
		p.add(freq);
		{ p.add(freq + Rand(tr, d.negated, d)) } ! 2;
		{ q.add(freq + Rand(tr, d.negated, d)) } ! 3
	};
	[p, q].collect(f) * 0.1 / n
}.OverlapTexture(4, 4, 2)
