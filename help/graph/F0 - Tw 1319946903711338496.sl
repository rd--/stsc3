(* https://twitter.com/redFrik/status/1319946903711338496 ; graph rewrite (rd) *)
var c = [0 0.804 1.944 3.084 3.888 5.028 5.832 6.972 7.776 8.916 10.056 10.86];
{ :tr |
	var i = PulseCount(tr, 0);
	var z = {
		var o = 12 * TRand(3, 6, tr);
		var n = Select(Choose(tr, [0, 2, 3, 5, 7, 8, 10]), c);
		var f = LfTri(1 / 99, 0) * 9 + (o + n).MidiCps;
		var y = SinOscFb(f, LfTri(1 + i / 50, 0) + 1 / 2);
		var e = TRand(0, 1, tr) > 0.5 / 2;
		EqPan2(y, LfTri(i / 70, 0)) * e
	} !+ 9;
	CombN(z, 0.2, 0.2, 1).Tanh * 0.1
}.OverlapTexture(3, 9, 2).Mix
