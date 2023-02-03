;; sturmian sequencer i (jrhb)
var rules = [[0, 1], [0]];
var rewrite = { :n |
	var r = [0];
	n.timesRepeat {
		r := r.collect({ :e |
			rules[e + 1]
		}).concatenation
	};
	r
};
var strFunc = { :i |
	var str = rewrite(i + 6);
	var dt = 2 ** i.negated * 10;
	var trig = TDmdFor(dt, 0, Lseq(1, str));
	var freq = ExpRand(200, i + 1 / 7 * 10100);
	Ringz(
		trig,
		freq * [1, 1.2, 1.5],
		ExpRand(2 ** i.negated * 0.1, 1.101)
	).sum.Distort
};
(0 .. 6).collect(strFunc).Splay2 * 0.3
