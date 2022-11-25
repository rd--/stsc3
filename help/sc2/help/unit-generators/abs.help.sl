# abs

Absolute value, _-1.abs = 1.abs = 1_.

	var a = Ln(-1, 1, 2);
	var b = a.abs;
	SinOsc([a, b] * 220 + [220, 440], 0) * [a, a.abs] * 0.1

Compare:

	var o = SyncSaw(100, 440);
	[o, o.abs] * 0.1

