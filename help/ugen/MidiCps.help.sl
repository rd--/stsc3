(* MidiCps *)
Saw(Line(24, 108, 10).MidiCps) * 0.05

(* MidiCps ; step *)
Saw(Line(24, 108, 10).RoundTo(1).MidiCps) * 0.05

(* ---- MidiCps ; requires=keywords *)
Saw(
	Line(
		start: 24,
		end: 108,
		dur: 10
	).MidiCps
) * 0.05

(* MidiCps ; step ; requires=keywords *)
Saw(
	Line(
		start: 24,
		end: 108,
		dur: 10
	).RoundTo(1).MidiCps
) * 0.05
