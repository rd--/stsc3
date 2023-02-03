"f0 <https://twitter.com/redFrik/status/1254441448327479299>"
| b e c d m l j f y o |
b := #(1 3 5 8 10).
e := {3. 2/3. 4. 3/2. 2}.
c := 0.021.
d := (LfTri freq: b / 999 iphase: 0) % 1.
m := LfTri freq: b * c iphase: 0.
l := m * 7 + 20 + (Lseq repeats: Float infinity list: (b % m * 5 + 6)).
j := Duty dur: (e / (12 ** m)) reset: 0 doneAction: 0 level: l.
f := (DegreeToKey bufnum: (b asLocalBuf) in: j octave: 12) midiCps.
y := Decay2 in: (Impulse freq: {2/3. 1.5. 3. 1.5. 3} phase: 0) attackTime: c decayTime: d mul: d.
o := SinOscFb freq: f feedback: (((LfTri freq: (c / b) iphase: 0) + 1) / 3) mul: y.
FreeVerb in: o splay2 mix: 0.1 room: 1 damp: 0.5 mul: 0.2

"rd (edit) ; f0 <https://twitter.com/redFrik/status/1254441448327479299>"
| b e c d m l j k o |
b := #(1 3 5 8 10).
e := {3. 2/3. 4. 3/2. 2}.
c := 0.021.
d := LfTri freq: b / 999 iphase: 0.
m := LfTri freq: b * c iphase: 0.
l := m * 7 + 20 + (Lseq repeats: Float infinity list: b % m * 5 + 6).
j := Duty dur: (e / (12 ** m)) reset: 0 doneAction: 0 level: l.
k := DegreeToKey bufnum: (b asLocalBuf) in: j octave: 12.
o := SinOscFb freq: k midiCps feedback: (LfTri freq: (c / b) iphase: 1 mul: d).
FreeVerb in: o splay2 mix: 0.1 room: 1 damp: 0.5 mul: 0.15
