(* https://sccode.org/1-4Qy ; f0 ; 0293 *)
{ :t :a |
	var d;
	{
		var i = a[1];
		var b = Saw(i);
		d := 1 / i;
		Release(
			EqPan(
				SinOscFb(DmdFor(d, 0, Dseq(1, a * 99 / 2)), b),
				b
			) / 5,
			0.02, 8, 0.02
		)
	}.playAt(t + 0.5);
	[2, a + 3 / d % 9]
}.scheduleInjecting((4 .. 12))
