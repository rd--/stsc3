(* https://sccode.org/1-4Qy ; f0 ; 0101 *)
{ :t :i |
	var c = i % 8 + 1;
	{
		var a = DelayN(InFb(2, 0), 1, 1);
		var o = SinOsc(c * 99 + [0, 2], a.reversed);
		Release(o / 9, 0, 9 - c, 16)
	}.playAt(t + 0.5);
	[9 - c, c]
}.scheduleInjecting(1)
