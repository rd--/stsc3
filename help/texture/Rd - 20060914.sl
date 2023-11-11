(* 20060914 ; rd *)
{
	var chord = { :m |
		var ds = 3;
		var du = [5, 4, 5, 7, 4, 5];
		var d = du * ds;
		var freq = XLine(m, m + Rand(0.05, 0.5), d).MidiCps;
		var env = Sine(1, du.max * ds) * Rand(0.005, 0.01);
		var pos = XLine(1.Rand2, 1.Rand2, d);
		var osc = SinOsc(freq, 0);
		EqPan2(osc, pos).sum * env
	};
	var scale = [0, 2, 4, 5, 7, 9, 11];
	var octaves = [4, 5, 6, 7];
	var mnn = scale.collect { :n |
		octaves.collect { :o |
			n + (o * 12)
		}
	}.concatenation;
	var chd = { mnn.atRandom } ! 6;
	{ chd.chord } !+ 9
}.overlap(21, 0, 3)
