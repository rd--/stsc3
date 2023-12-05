(* https://twitter.com/headcube/status/528187147543511041 (nv) *)
(1 .. 4).collect { :j |
	var f = 32 ^ (1 .. 10).collect { :i |
		var n = LfNoise0(0.25 ^ (j - 1) / 8);
		var p = LfPulse(2 ^ (i - 1) / 8, 0, 0.5);
		1 / 4 ^ (i - 1) * (n > 0 - p).Abs
	}.Sum * 30;
	EqPan2(Rlpf(Pulse(f, 0.3), Lag(f.Sqrt, 2) * 30, 0.5), 0) * 0.2
}.Mix * 0.25
