(* 20060922 ; rd ; requires=Perc *)
var t0 = Impulse(1 / 0.3, 0);
var t1 = [t0, TDelay(t0, 0.15)];
var k = Rand(t1, 56, 57);
SinOsc(
	MulAdd(
		SinOsc(k.MidiCps, 0),
		Rand(t1, 40, 480),
		(k + 1 + Rand(t1, -1, 1)).MidiCps
	),
	0
) * Perc(t1, 0.01, 0.9, -4) * 0.1
