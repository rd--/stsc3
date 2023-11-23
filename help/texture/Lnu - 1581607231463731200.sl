(* https://twitter.com/lukiss163/status/1581607231463731200 ; lnu ; requires=SfAcquire *)
{
	var sf = SfAcquireMono('floating_1');
	var n = [-36 -9 -14 0 -19 -5 3 -2 -24 -7];
	var k = n.size;
	var r = { LfdNoise3(1 / 86).Abs };
	var w = r:/0 ! k * Warp1(
		1,
		sf,
		r:/0 ! k,
		n.MidiRatio,
		r:/0 ! k * 8 + 8 / 86,
		-1,
		12,
		r:/0 ! k / 4,
		4
	);
	LeakDc(FreeVerb(w, r:/0 ! k, r:/0 ! k + 0.5, r:/0 ! k), 0.995).Splay2.Tanh
}.overlap(4, 5, 2)
