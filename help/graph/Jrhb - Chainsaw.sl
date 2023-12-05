(* chainsaw (jrhb) *)
var f = { :saw |
	var rate = ExpRand(0.1, 2) ! 2;
	var freq1 = 0.6.coin.if {
		LfNoise1(rate).ExpRange(0.01, 10)
	} {
		LfNoise1(rate).ExpRange(10, 50)
	};
	var freq2 = saw.ExpRange(freq1, freq1 * LfNoise1(rate).ExpRange(2, 10));
	var u1 = LfSaw(freq2, 0);
	var u2 = 0.5.coin.if {
		u1 * [1 - saw, saw.reversed].atRandom
	} {
		u1 * (LfSaw(freq1 * 0.1, 0) * 0.1 + 1)
	};
	u2.Clip2(1.0)
};
var compose = { :f:/1 :g:/1 |
	{ :x |
		f(g(x))
	}
};
var g = { :func :n |
	var r = func;
	n.timesRepeat {
		r := compose(r, r)
	};
	r
};
var freq3 = g(f:/1, 4).value(LfSaw(0.2 * [1, 1.1], 0)).ExpRange(6, 11000);
var osc = Bpf(Saw(freq3).product, [70, 800, 9000, 5242], 0.2).Sum;
[osc, osc] * 0.3
