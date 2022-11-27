;; https://sccode.org/1-4Qy ; f0 ; 0293
{ :a |
	var d;
	{
		var i = a[1];
		var b = Saw(i);
		d := 1 / i;
		Release(Pan2(SinOscFB(DmdFor(d, 0, Seq(1, a * 99 / 2)), b), b, 1 / 5), 0.02, 8, 0.02)
	}.play;
	[2, a + 3 / d % 9]
}.scheduleInjecting([4 .. 12])
