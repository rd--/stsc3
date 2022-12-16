;; MidiCps
Saw(Ln(24, 108, 10).MidiCps) * 0.05

;; MidiCps ; step
Saw(Ln(24, 108, 10).RoundTo(1).MidiCps) * 0.05

;; MidiCps ; keywords
Saw(
	Ln(
		start: 24,
		end: 108,
		dur: 10
	).MidiCps
) * 0.05

;; MidiCps ; step ; keywords
Saw(
	Ln(
		start: 24,
		end: 108,
		dur: 10
	).RoundTo(1).MidiCps
) * 0.05
