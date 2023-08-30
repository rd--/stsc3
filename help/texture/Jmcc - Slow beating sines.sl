(* slow beating sines (jmcc) #7 *)
{
	var n = 20; (* n * 3 components in each channel *)
	var d = 5.0; (* beating frequency deviation *)
	var p = [];
	var q = [];
	n.timesRepeat {
		var freq = IRand(24, 84).MidiCps;
		p.add(freq);
		{ p.add(freq + d.Rand2) } ! 2;
		{ q.add(freq + d.Rand2) } ! 3
	};
	[p, q].collect { :freq |
		SinOscBank(
			freq,
			0.1,
			{ 2 * pi.Rand } ! (3 * n)
		)
	} / n
}.xfade(4, 4)
