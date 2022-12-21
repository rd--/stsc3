;; sturmian sequencer iii (jrhb)
var rules = [[0, 1], [0]];
var rewrite = { :c :n |
	var r = c;
	n.timesRepeat {
		r := r.collect({ :e |
			rules[e + 1]
		}).concatenation
	};
	r
};
var n = 9;
var x = MouseX(1, SampleRate(), 1, 0.2);
var strFunc = { :i |
	var str = rewrite([0], i + 6);
	var dt = 1 / SampleRate() / (n - i + 2) * x;
	TDmdFor(dt, 0, Seq(inf, str - 0.5))
};
(0 .. n - 1).collect(strFunc).Splay2 * 0.3
