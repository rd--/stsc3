"strummable guitar (jmcc) #11"
| scale str strs |
scale := #(52 57 62 67 71 76).
str := [ :i |
 | x t e n dt s |
 x := MouseX minval: 0 maxval: 1 warp: 0 lag: 0.2.
 t := (HPZ1 in: (x > (0.25 + (i - 1 * 0.1)))) abs.
 e := (Decay in: t decayTime: 0.05) kr.
 n := PinkNoise mul: e.
 dt := (scale at: i) midiCps reciprocal.
 s := CombL in: n maxdelaytime: dt delaytime: dt decaytime: 4.
 Pan2 in: s pos: (i - 1 * 0.2 - 0.5) level: 1].
strs := (1 to: scale size) mixFill: str.
LeakDC in: (LPF in: strs freq: 12000) coef: 0.995
