(* https://twitter.com/redFrik/status/1591161283591782401 *)
{ :nextDelay |
	var d = 1 / 12;
	var b = (1 .. 8) / 4;
	var l = LfTri(d, b) / 8;
	var m = { [0, 2, 5, 7, 9].atRandom + [36, 48].atRandom } ! 4 + l;
	var y = VarSaw(m.MidiCps, 0, l + 0.4);
	var z = Rlpf(y, 8 ^ LfTri(d * b, 0) * 999, 1);
	var a = AllpassC(z, 1, LfTri([6, 4, 6] / 4, [0, 1]) + 1 / 2, 1);
	var x = AllpassC(a * d, 1, 1 / 2, 4).Splay;
	Release(x, 9, nextDelay, 24)
}.playEvery { 9.randomFloat / 8 + 33.randomFloat + 8 }
