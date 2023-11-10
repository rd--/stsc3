"TRand"
| t f |
t := Dust density: #(5 12).
f := TRand lo: #(200 1600) hi: #(500 3000) trig: t.
SinOsc freq: f kr phase: 0 mul: 0.1
