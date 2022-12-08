"harmonic zither (jmcc) #11"
| pitch triggerSpacing panSpacing stringFunc |
pitch := #(50 53.86 57.02 59.69 62 64.04 65.86 67.51 69.02 71.69 72.88 74).
triggerSpacing := 0.5 / (pitch size - 1).
panSpacing := 1.5 / (pitch size - 1).
stringFunc := [ :i |
	| trigger pluck period string |
	trigger := Hpz1 in: ((MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2) > (0.25 + (i - 1 * triggerSpacing))) abs.
	pluck := PinkNoise mul: (Decay in: trigger decayTime: 0.05) kr.
	period := (pitch at: i) midiCps reciprocal.
	string := CombL in: pluck maxdelaytime: period delaytime: period decaytime: 8.
	Pan2 in: string pos: i - 1 * panSpacing - 0.75 level: 1
].
LeakDc in: (Lpf in: (1 to: pitch size collect: stringFunc) sum freq: 12000) coef: 0.995
