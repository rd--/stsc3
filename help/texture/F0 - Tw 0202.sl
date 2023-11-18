(* https://sccode.org/1-4Qy ; f0 ; 0202 *)
{ :currentTime :each |
	{
		var b = 0.1;
		var s = Dseq(9, [0, each % 9, 2, 3, 4, 0, 2, 1] * 150);
		var f = DmdFor(b, 0, s);
		var p = SinOsc(f, 0);
		EqPan(SinOsc(each, p), each % 3 - 1) * b
	}.playAt(currentTime + 0.5);
	[each % 5 + 1, each + 1]
}.scheduleInjecting(1)
