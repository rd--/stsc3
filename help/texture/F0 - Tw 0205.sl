;; https://sccode.org/1-4Qy ; f0 ; 0205
{ :i |
	var a = Wrap(i * 9, 99, 8000);
	{
		Release(SinOsc(DmdFor(1 / [8, 9], 0, Dseq(99, a)), 0) * 0.1, 0, 0, 9)
	}.play;
	a
}.recurseEvery([1, 3, 5, 7, 9] * 99, 2)
