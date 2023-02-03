;; https://sccode.org/1-4Qy ; f0 ; 0202
{ :i |
	{
		var b = 0.1;
		var p = SinOsc(DmdFor(b, 0, Lseq(9, [0, i % 9, 2, 3, 4, 0, 2, 1] * 150)), 0);
		EqPan2(SinOsc(i, p), i % 3 - 1) * b
	}.play;
	[i % 5 + 1, i + 1]
}.scheduleInjecting(1)
