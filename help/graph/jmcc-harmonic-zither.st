"harmonic zither (jmcc) #11"
| pitch mousex mousey triggerSpacing panSpacing out |
pitch := #(50 53.86 57.02 59.69 62 64.04 65.86 67.51 69.02 71.69 72.88 74).
mousex := MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2.
mousey := MouseY minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2.
triggerSpacing := 0.5 / (pitch size - 1).
panSpacing := 1.5 / (pitch size - 1).
out := pitch size mixFill: [:i |
 | trigger pluck period string |
 trigger := HPZ1 in: (mousex > (0.25 + ((i - 1) * triggerSpacing))) abs.
 pluck := PinkNoise mul: (Decay in: trigger decayTime: 0.05) kr.
 period := (pitch at: i) midicps reciprocal.
 string := CombL in: pluck maxdelaytime: period delaytime: period decaytime: 8.
 Pan2 in: string pos: ((i - 1) * panSpacing - 0.75) level: 1 ].
LeakDC in: (LPF in: out freq: 12000) coef: 0.995
