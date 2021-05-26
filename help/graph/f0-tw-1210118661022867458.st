"f0 ; https://twitter.com/redFrik/status/1210118661022867458"
| b d g l f o c p |
b := (2 to: 7) mce reciprocal.
d := 48 reciprocal.
g := 0 cos cos.
l := #(0 -16 0 -16 0 0 -16 0 -16 0 -16 0 0 0 -16 -16 47 0 -16 0 -16 0 -16 0 0 -16 0 0 -16 0 0 0 0 0 -5 -16 -16 -15) mce.
f := Duty dur: b reset: 0 doneAction: 0 level: (Dseq repeats: #dinf list: l + 48).
o := SinOscFB freq: f feedback: ((SinOsc freq: d phase: 0) max: 0) mul: 0 cos sin.
c := SinOsc freq: 0 phase: (Float pi constant raisedTo: o).
p := PitchShift in: c windowSize: g pitchRatio: b reciprocal pitchDispersion: d timeDispersion: d mul: b.
LeakDC in: (Splay input: c + p) coef: 0.995 mul: g
