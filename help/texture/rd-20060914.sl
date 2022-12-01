;; 20060914 ; rd
{
	var chrd = { :m |
		var ds = 3;
		var du = [5, 4, 5, 7, 4, 5];
		var d = du * ds;
		var freq = XLn(m, m + 0.05.rrand(0.5), d).MidiCps;
		var env = Sine(1, du.max * ds) * 0.005.rrand(0.01);
		var pos = XLn(1.0.rand2, 1.0.rand2, d);
		var osc = SinOsc(freq, 0);
		Pan2(osc, pos, env).sum
	};
	var scale = [0, 2, 4, 5, 7, 9, 11];
	var octaves = [4, 5, 6, 7];
	var mnn = scale.collect({ :n | octaves.collect({ :o | n + (o * 12) }) }).concatenation;
	var chd = { mnn.atRandom } ! 6;
	{ chrd(chd) }.dup(9).sum
}.overlap(21, 0, 3)
