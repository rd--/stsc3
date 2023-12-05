(* https://sccode.org/1-4Qy ; f0 ; 0246 *)
var b = (1 .. 9) * 1.5;
var d = [2 / b, 3 / b];
var e = LfPulse(d, 0, 0.5);
CombC(
	Ringz(e, (LfPulse(1 / b, 0, 0.5) * b + 50).MidiCps, b / 9).Sum * 0.0002,
	2,
	2 - Lpf(e, 50),
	1
).transposed.Mix
