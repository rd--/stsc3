"DmdOn"
| n t f o |
n := (Seq repeats: Float infinity list: #(0 2 4 5 7 9 11 12)) + ((TRand lo: 3 hi: 6 trig: (Dust density: 9)) rounded * 12).
t := Impulse freq: 10 phase: 0.
f := DmdOn trig: t reset: 0 demandUGens: n midiCps.
o := SinOsc freq: {f. f + 0.7} phase: 0.
o cubed cubed * 0.1

"DmdOn"
| n t f o |
n := Diwhite length: Float infinity lo: 60 hi: 72.
t := Impulse freq: 10 phase: 0.
f := DmdOn trig: t reset: 0 demandUGens: n midiCps.
o := SinOsc freq: {f. f + 0.7} phase: 0.
o cubed cubed * 0.1

"----"
(DmdOn trig: 0 reset: 1 demandUGens: 2) inspect
