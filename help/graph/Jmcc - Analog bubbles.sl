(* analog bubbles ; jmcc *)
var f = MulAdd(
	LfSaw(0.4, 0),
	24,
	MulAdd(LfSaw([8, 7.23], 0), 3, 80)
).MidiCps; (* glissando function *)
CombN(SinOsc(f, 0) * 0.05, 0.2, 0.2, 4) (* echoing sine wave *)
