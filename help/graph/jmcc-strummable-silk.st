"strummable silk (jmcc) #11"
| x o s |
x := MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2.
o := 1 to: 8 collect: [ :ix |
  | n trigger env pluck freq z metal |
  n := 15.
  trigger := (HPZ1 in: (x > (0.25 + (ix - 1 * 0.07)))) abs.
  env := Decay in: (Impulse freq: 14 phase: 0 mul: (Lag in: (Trig in: trigger dur: 1) lagTime: 0.2 mul: 0.01) kr) decayTime: 0.04.
  pluck := PinkNoise mul: env.
  freq := ((#(-2 0 3 5 7 10 12 15) at: ix) + 60) midiCps.
  metal := RingzBank input: pluck freq: (n iota collect: [ :j | j * freq ]) amp: #(1) time: ([ 0.3 rand: 1.0 ] dup: n).
  Pan2 in: metal pos: (ix - 1 * 0.2 - 0.5) level: 1
].
s := LeakDC in: (LPF in: o sum freq: 12000) coef: 0.995.
6 timesRepeat: [ s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand. 0.05 rand} decaytime: 4 ]. "rvb"
s

"strummable silk (jmcc) #11 ; Klank"
| x o s |
x := MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2.
o := 1 to: 8 collect: [ :ix |
  | n trigger env pluck freq z metal |
  n := 15.
  trigger := (HPZ1 in: (x > (0.25 + ((ix - 1) * 0.07)))) abs.
  env := Decay in: (Impulse freq: 14 phase: 0 mul: (Lag in: (Trig in: trigger dur: 1) lagTime: 0.2 mul: 0.01) kr) decayTime: 0.04.
  pluck := PinkNoise mul: env.
  freq := ((#(-2 0 3 5 7 10 12 15) at: ix) + 60) midiCps.
  z := {n iota collect: [ :j | j * freq ]. nil. [ 0.3 rand: 1.0 ] dup: n} asKlankSpec.
  metal := Klank input: pluck freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: z.
  Pan2 in: metal pos: ((ix - 1) * 0.2 - 0.5) level: 1
].
s := LeakDC in: (LPF in: o sum freq: 12000) coef: 0.995.
6 timesRepeat: [ s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand. 0.05 rand} decaytime: 4 ]. "rvb"
s
