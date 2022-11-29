;; phase modulation with slow beats (jmcc) #6
{
	var x = MouseX(100, 6000, 1, 0.2); (* random frequency of new events *)
	var y = MouseY(0, 2, 0, 0.2); (* modulation index *)
	var f1 = Rand(0, x);
	var ph = 0;
	3.timesRepeat({
		var f2 = Rand(0, x);
		ph := SinOsc([f2, f2 + 1.0.rand2], 0) * y + ph
	});
	SinOsc([f1, f1 + 1.0.rand2], ph) * 0.1
}.overlap(4, 4, 4)