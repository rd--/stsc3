;; chainsaw (jrhb)
var f = { :saw |
	var rate = ExpRand(0.1, 2) ! 2;
	var freq = 0.6.coin.if { LinExp(LFNoise1(rate), -1, 1, 0.01, 10) } { LinExp(LFNoise1(rate), -1, 1, 10, 50) };
	var u1 = LFSaw(LinExp(saw, -1, 1, freq, freq * LinExp(LFNoise1(rate), -1, 1, 2, 10)), 0);
	var u2 = 0.5.coin.if { u1 * [1 - saw, saw.reversed].atRandom } { u1 * (LFSaw(freq * 0.1, 0) * 0.1 + 1) };
	u2.Clip2(1.0)
};
var compose = { :f :g | { :x | f(g(x)) } };
var g = { :func :n |
	var r = func;
	n.timesRepeat { r := compose(r, r) };
	r
};
var freq = LinExp(g(f, 4).value(LFSaw(0.2 * [1, 1.1], 0)), -1, 1, 6, 11000);
var osc = BPF(Saw(freq).product, [70, 800, 9000, 5242], 0.2).sum;
[osc, osc] * 0.3
