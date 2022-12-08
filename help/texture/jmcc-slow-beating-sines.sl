;; slow beating sines (jmcc) #7 - ritual hymn in praise of the god of the LS-3000 life support unit
{
	var n = 20; (* n * 3 components in each channel *)
	var d = 5.0; (* beating frequency deviation *)
	var p = OrderedCollection.new;
	var q = OrderedCollection.new;
	var f = { :freq |
		SinOscBank(freq.asArray, 0.1, { 2 * pi.Rand } ! (3 * n))
	};
	n.timesRepeat {
		var freq = IRand(24, 84).MidiCps;
		p.add(freq);
		{ p.add(freq + d.Rand2) } ! 2;
		{ q.add(freq + d.Rand2) } ! 3
	};
	[p, q].collect(f) / n
}.xfade(4, 4)
