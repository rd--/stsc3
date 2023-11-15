(* https://twitter.com/redFrik/status/23182604046 ; f0 ; 0011 *)
var s = { :o :i |
	SinOsc([i, i + 0.0001] ^ 2 * f(o, i - 1), f(o, i - 1) * 0.0001) * f(o, i - 1)
};
var f = { :o :i |
	(i > 0).if { s(o, i) } { o }
};
var k = 10 + 10.atRandom; (* udp packet size *)
f(k, 6) / k
