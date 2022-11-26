;; https://twitter.com/redFrik/status/1591161283591782401
{ :t |
	var d = 1 / 12;
	var b = [1 .. 8] / 4;
	var l = LFTri(d, b) / 8;
	var y = VarSaw(({ [0, 2, 5, 7, 9].atRandom + [36, 48].atRandom } ! 4 + l).midiCps, 0, l + 0.4);
	var z = RLPF(y, 8 ** LFTri(d * b, 0) * 999, 1);
	var x = Splay2(AllpassC(AllpassC(z, 1, LFTri([6, 4, 6] / 4, [0, 1]) + 1 / 2, 1), 1, 1 / 2, 4) * d);
	Release(x, 9, t, 24)
}.playEvery { 9.random / 8 + 33.random + 8 }
