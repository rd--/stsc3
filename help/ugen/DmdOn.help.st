"DmdOn"
| n t f o |
n := (Seq repeats: #dinf list: #(0 2 4 5 7 9 11 12)) + ((TRand lo: 3 hi: 6 trig: (Dust density: 9)) rounded * 12).
t := Impulse freq: 10 phase: 0.
f := DmdOn trig: t reset: 0 demandUGens: n midicps.
o := SinOsc freq: (Mce with: f with: f + 0.7) phase: 0.
o cubed cubed * 0.1

"DmdOn"
| n t f o |
n := Diwhite length: #dinf lo: 60 hi: 72.
t := Impulse freq: 10 phase: 0.
f := DmdOn trig: t reset: 0 demandUGens: n midicps.
o := SinOsc freq: (Mce with: f with: f + 0.7) phase: 0.
o cubed cubed * 0.1

"----"
(DmdOn trig: 0 reset: 1 demandUGens: 2) inspect
