"Demand"
|n t f o|
n := Diwhite length: #dinf lo: 60 hi: 72.
t := Impulse freq: 10 phase: 0.
f := (Demand trig: t reset: 0 demandUGens: n midicps) kr.
o := SinOsc freq: {f. f + 0.7} phase: 0.
o cubed cubed * 0.1

"Demand ; rate: notation"
|n t f o|
n := Diwhite dr: {#dinf. 60. 72}.
t := Impulse kr: {10. 0}.
f := Demand kr: {t. 0. n midicps}.
o := SinOsc ar: {{f. f + 0.7}. 0}.
o cubed cubed * 0.1

"Demand ; rate: notation ; audio rate"
|n t f o|
n := Diwhite dr: {#dinf. 60. 72}.
t := Impulse ar: {10. 0}.
f := Demand ar: {t. 0. n midicps}.
o := SinOsc ar: {{f. f + 0.7}. 0}.
o cubed cubed * 0.1

"----"
(Demand trig: 0 reset: 1 demandUGens: 2) inspect
(Demand at: #kr trig: 0 reset: 1 demandUGens: 2) inspect
(Demand kr: {0. 1. 2}) inspect
