"synthetic piano (jmcc) #3"
| p |
p := [
	| n f ph s e c l |
	n := 36 rand: 90.
	f := 0.1 rand: 0.5.
	ph := 0.0 rand: 1.0.
	s := Impulse freq: f phase: ph mul: 0.1.
	e := Decay2 in: s attackTime: 0.008 decayTime: 0.04.
	c := [ :ix |
		| n0 o dt |
		n0 := LFNoise2 freq: 3000.
		o := #(-0.05 0.0 0.04) at: ix.
		dt := (n + o) midiCps reciprocal.
		CombL in: (n0 * e) maxdelaytime: dt delaytime: dt decaytime: 6
	].
	l := ((n - 36) / 27) - 1.
	Pan2 in: (1 to: 3 collect: c) sum pos: l level: 1
].
(p dup: 6) sum
