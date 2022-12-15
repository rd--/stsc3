;; https://twitter.com/headcube/status/528187147543511041 (nv)
var x = { :j |
	var y = { :i |
		1 / 4 ** (i - 1) * (LfNoise0(0.25 ** (j - 1) / 8) > 0 - LfPulse(2 ** (i - 1) / 8, 0, 0.5)).Abs
	};
	var f = 32 ** (1 .. 10).collect(y).sum * 30;
	EqPan2(Rlpf(Pulse(f, 0.3), Lag(f.Sqrt, 2) * 30, 0.5), 0) * 0.2
};
(1 .. 4).collect(x).sum * 0.25
