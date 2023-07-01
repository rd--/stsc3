# Abs -- absolute value

Contrary motion:

	var a = Ln(-1, 1, 2);
	var b = a.Abs;
	SinOsc([a, b] * 220 + [220, 440], 0) * [a, b] * 0.1

Compare:

	var o = SyncSaw(100, 440);
	[o, o.Abs] * 0.1
