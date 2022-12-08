"http://sccode.org/1-4Qy (f0)"
| f p m b |
f := (SinOsc freq: {1/5. 1/6} phase: 0) + #(798 912).
p := (SinOsc freq: (1/16) phase: 0 mul: 19 add: 99) * (SinOsc freq: #(9 8) phase: 0).
m := SinOsc freq: (SinOsc freq: 6 phase: 0 mul: (SinOsc freq: 0.009 phase: 0)) phase: 0.
b := SinOsc freq: f phase: p * m.
(SinOsc freq: #(201 301) phase: b) tanh * 0.1
