(* https://sccode.org/1-4Qy ; f0 ; 0333 *)
(0 .. 7).collect { :i |
	var b = i + 2 * 99;
	var f = SinOscFb(i + 1 / 150, 0).RoundTo(1) + 1 + i * 99 + SinOscFb([3, 2], 0);
	(Formant(f, b, b) * SinOscFb(i + 1 / 130, 0).Max(0)).Tanh
}.Mix / 3
