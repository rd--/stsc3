"bidirectional strummable guitar (jmcc) #11"
|pitch1 pitch2 mousex nd out|
pitch1 := #(52 57 62 67 71 76).
pitch2 := pitch1 + 7.
mousex := MouseX minval: 0 maxval: 1 warp: 0 lag: 0.2.
nd := [ :i |
	| trigger pluck1 period1 string1 pluck2 period2 string2 |
	trigger := HPZ1 in: mousex > (0.25 + (i * 0.1)).
	pluck1 := PinkNoise mul: (Decay in: (trigger max: 0) decayTime: 0.05) kr.
	period1 := (pitch1 at: i) midiCps reciprocal.
	string1 := CombL in: pluck1 maxdelaytime: period1 delaytime: period1 decaytime: 4.
	pluck2 := BrownNoise mul: (Decay in: (trigger negated max: 0) decayTime: 0.05) kr.
	period2 := (pitch2 at: i) midiCps reciprocal.
	string2 := CombL in: pluck2 maxdelaytime: period2 delaytime: period2 decaytime: -4.
	Pan2 in: (string1 + string2) pos: i * 0.2 - 0.5 level: 1].
out := 1 to: pitch1 size collect: nd.
LeakDC in: (LPF in: out sum freq: 12000) coef: 0.995
