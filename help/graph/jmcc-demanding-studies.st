"demanding studies (jmcc)"
| s1 s2 s3 x tr f o1 o2 o3 |
s1 := Choose repeats: Float infinity list: #(72 75 79 82).
s2 := Choose repeats: 1 list: #(82 84 86).
s3 := Seq repeats: Float infinity list: {72. 75. 79. s2}.
x := MouseX minval: 5 maxval: 13 warp: 0 lag: 0.2.
tr := Impulse freq: x phase: 0.
f := DmdOn trig: tr reset: 0 demandUGens: {(s1 - 12) midiCps dr. s3 midiCps dr}.
o1 := SinOsc freq: (f + #(0 0.7)) kr phase: 0.
o2 := Saw freq: (f + #(0 0.7)) kr mul: 0.3.
o3 := (o1 + o2) distort log distort cubed.
o3 * 0.1
