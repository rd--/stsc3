(* sturmian sequencer ii (jrhb) *)
var rules = [[0, 1], [0]];
var rewrite = { :c :n |
	var r = c;
	n.timesRepeat {
		r := r.collect { :e |
			rules[e + 1]
		}.concatenation
	};
	r
};
var n = 7;
(0 .. n - 1).collect { :i |
	var str = rewrite([0], i + 6);
	var dt = 2 ^ (n - i).negated * 20;
	var trig = TDmdFor(dt, 0, Dseq(1, str));
	var freq = TExpRand(200, (n - i) / n * 10100, trig);
	var trigFlt = Bpf(trig, LfNoise2(0.1) * 0.02 + 1 * freq, 0.2);
	Ringz(
		trigFlt,
		freq * [1, 1.1, 1.2],
		ExpRand(2 ^ i.negated * 0.1, 0.5)
	).Sum.Distort
}.Splay * 0.5
