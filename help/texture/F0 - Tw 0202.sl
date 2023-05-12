;; https://sccode.org/1-4Qy ; f0 ; 0202
{ :t :i |
	{
		var b = 0.1;
		var s = Dseq(9, [0, i % 9, 2, 3, 4, 0, 2, 1] * 150);
		var f = DmdFor(b, 0, s);
		var p = SinOsc(f, 0);
		EqPan2(SinOsc(i, p), i % 3 - 1) * b
	}.playAt(t + 0.5);
	[i % 5 + 1, i + 1]
}.scheduleInjecting(1)
