(* f0 ; https://twitter.com/redFrik/status/1136928201886904320 *)
[
	0 -> LfTri:/2,
	1 -> LfSaw:/2,
	2 -> LfPar:/2
].collect { :a |
	var ix = a.key;
	var osc:/2 = a.value;
	var b = [4, 2, 1, 3, 5];
	var c = (osc(1 / b, 0) * b).RoundTo(osc(b / 9, 0) > 0);
	var o = SinOscFb(c ^ 2 * ((osc(0.02, ix) > 0) + 1 * 50), osc(c / 9, 0) % 1);
	var s = o * (osc(1 / b * osc(b / 99, ix), 0)).Max(0) * osc(0.01, ix);
	var x = AllpassC(s, 1, Wrap(c, 0.5, 1), 2);
	var y = Bpf(x, ix * 99 + 400, 0.001) * (osc(0.04, ix) + 1 * 9);
	Splay2(x + y) / 3
}.Mix
