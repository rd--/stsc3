"DmdFor ; c.f. equivalent DmdOn graph ; (Duty)"
| n t f o |
t := Dust density: 9.
n := (Seq repeats: Float infinity list: #(0 2 4 5 7 9 11 12)) + ((TRand lo: 3 hi: 6 trig: t) rounded * 12).
f := DmdFor dur: 0.1 reset: 0 level: n midiCps.
o := SinOsc freq: {f. f + 0.7} phase: 0.
o cubed cubed * 0.1
