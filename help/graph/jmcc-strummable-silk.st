"strummable silk (jmcc) #11"
[|x o s|
 x := MouseX minval: 0.0 maxval: 1.0 warp: 0.0 lag: 0.2 .
 o := 8 mixFill: [:i|
  |n trigger pluck freq z metal|
  n := 15 .
  trigger := (HPZ1 in: (x > (0.25 + ((i - 1) * 0.07)))) abs .
  pluck := PinkNoise mul: (Decay in: (Impulse freq: 14 phase: 0 mul: (Lag in: (Trig in: trigger dur: 1) lagTime: 0.2 mul: 0.01) kr) decayTime: 0.04) .
  freq := (({ -2 . 0 . 3 . 5 . 7 . 10 . 12 . 15 } at: i) + 60) midicps .
  z := { n arrayFill: [:j | j * freq] . n replicate: 1 . n arrayFill: [:_j| 0.3 rand: 1.0] } transpose concatenation .
  metal := Klank input: pluck freqscale: 1 freqoffset: 0 decayscale: 1 specificationsArrayRef: z .
  Pan2 in: metal pos: ((i - 1) * 0.2 - 0.5) level: 1 .] .
 s := LeakDC in: (LPF in: o freq: 12000) coef: 0.995 .
 (1 to: 6) do: [:_i| s := AllpassN in: s maxdelaytime: 0.1 delaytime: {0.05 rand . 0.05 rand} decaytime: 4] . "rvb"
 s] value
