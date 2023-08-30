(* slow beating harmonic sines (jmcc) #7 *)
{
	var n = 8; (* number of notes *)
	var d = 0.4; (* beat frequency deviation *)
	var m = 5; (* harmonics per note *)
	var p = [];
	var q = [];
	var k = 24 + 12.atRandom;
	n.timesRepeat {
		var freq = ([0, 2, 4, 5, 7, 9].atRandom + (IRand(0, 7) * 12) + k).MidiCps;
		[p, q].do { :each |
			[1, 2, 4, 5, 6].do { :h |
				each.add(freq * h + d.Rand2)
			}
		}
	};
	[p, q].collect { :freq |
		SinOscBank(freq, 0.1, { 2 * Rand(0, pi) } ! (m * n))
	} / n
}.xfade(6, 3)
