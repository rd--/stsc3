(* tw 0033 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537 *)
var f = LfPar(1 / 14, 0).RoundTo(1) * 20 + 80;
var a = Pulse([1 .. 4], 0.35);
var n = BrownNoise() * a;
var z = (1 .. 4).collect { :i |
	[i + 1 * f, i * f + i + 0.333]
};
var o = LfPar(z, 0);
(o > n / 3).Splay2.Mix * 0.1
