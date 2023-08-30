(* 20060922 ; rd ; requires=Perc *)
var t0 = Impulse(1 / 0.3, 0);
var t1 = [t0, TDelay(t0, 0.15)];
var k = TRand(56, 57, t1);
SinOsc(
	MulAdd(
		SinOsc(k.MidiCps, 0),
		TRand(40, 480, t1),
		(k + 1 + TRand(-1, 1, t1)).MidiCps
	),
	0
) * Perc(t1, 0.01, 0.9, -4) * 0.1
