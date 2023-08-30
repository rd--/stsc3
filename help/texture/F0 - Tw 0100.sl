(* https://sccode.org/1-4Qy ; f0 ; 0100 *)
{
	var z = IRand(1, 9) * 99;
	var a = DelayN(Lpf(InFb(2, 0), z), 0.2, 0.2);
	Release(SinOsc(z + [0, 3], a * pi) / 5, 0, 6, 9)
}.playEvery(6)
