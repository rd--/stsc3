(* phase modulation with slow beats (jmcc) #6 *)
{
	var x = MouseX(100, 6000, 1, 0.2); (* random frequency of new events *)
	var y = MouseY(0, 2, 0, 0.2); (* modulation index *)
	var f1 = x.Rand;
	var ph = 0;
	3.timesRepeat {
		var f2 = x.Rand;
		ph := SinOsc([f2, f2 + 1.Rand2], 0) * y + ph
	};
	SinOsc([f1, f1 + 1.Rand2], ph) * 0.1
}.overlap(4, 4, 4)
