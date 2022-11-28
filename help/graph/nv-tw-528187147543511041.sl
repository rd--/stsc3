;; https://twitter.com/headcube/status/528187147543511041 (nv)
var x = { :j |
	var y = { :i |
		1 / 4 ** (i - 1) * (LfNoise0(0.25 ** (j - 1) / 8) > 0 - LfPulse(2 ** (i - 1) / 8, 0, 0.5)).abs
	};
	var f = 32 ** (1 .. 10).collect(y).sum * 30;
	Pan2(Rlpf(Pulse(f, 0.3), Lag(f.sqrt, 2) * 30, 0.5), 0, 0.2)
};
(1 .. 4).collect(x).sum * 0.25
