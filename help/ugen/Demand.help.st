"Demand"
| n t f o |
n := Diwhite length: #dinf lo: 60 hi: 72.
t := Impulse freq: 10 phase: 0.
f := (Demand trig: t reset: 0 demandUGens: n midicps) kr.
o := SinOsc freq: (Mce with: f with: f + 0.7) phase: 0.
o cubed cubed * 0.1

"Demand ; audio rate"
| n t f o |
n := Diwhite length: #dinf lo: 60 hi: 72.
t := Impulse freq: 10 phase: 0.
f := Demand trig: t reset: 0 demandUGens: n midicps.
o := SinOsc freq: (Mce with: f with: f + 0.7) phase: 0.
o cubed cubed * 0.1

"----"
(Demand trig: 0 reset: 1 demandUGens: 2) inspect
