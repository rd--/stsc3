"http://sccode.org/1-4Qy (f0)"
| f u d i p |
f := SinOsc freq: {1/8. 1/9} phase: 0 mul: 4 add: #(400 202).
u := (SinOsc freq: (1/9) phase: 0) + 1 / 88.
d := (SinOsc freq: (1/8) phase: 0) + 1 / 99.
i := InFb numChan: 1 bus: #(1 0).
p := CombC in: (LagUD in: i lagTimeU: u lagTimeD: d) maxdelaytime: 1 delaytime: 0.08 decaytime: 9.
SinOsc freq: f phase: p mul: 0.15
