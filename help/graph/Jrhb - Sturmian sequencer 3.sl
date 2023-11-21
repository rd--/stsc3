(* sturmian sequencer iii (jrhb) *)
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
var n = 9;
var x = MouseX(1, SampleRate(), 1, 0.2);
(1 .. n).collect { :i |
	var str = rewrite([0], i + 5);
	var dt = 1 / SampleRate() / (n - i + 1) * x;
	TDmdFor(dt, 0, Dseq(inf, str - 0.5))
}.Splay * 0.3
