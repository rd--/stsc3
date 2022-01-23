"f0 ; https://twitter.com/redFrik/status/1395040511795372038"
| b c l f o p m h q r |
b := #(2 6 3 1).
c := 500.
l := Seq repeats: Float infinity list: ((SinOscFB freq: b / 24 feedback: 0) rounded + (0 to: 9) * 7 + 29.17) midiCps.
f := DmdFor dur: b reset: (SinOscFB freq: b / 9 feedback: 0) level: l.
o := SinOscFB freq: f feedback: (((SinOscFB freq: b / 13 feedback: 0) max: 0) / 3 * 2).
p := AllpassN in: o / 8 maxdelaytime: 1 delaytime: 0.51 decaytime: 8.
m := ((SinOscFB freq: b / c feedback: 0) max: 0) / c.
h := Hasher in: ((SinOscFB freq: b / 12 feedback: 0) max: 0) mul: m.
q := BPF in: h freq: (2 ** (SinOscFB freq: b / c * 2 feedback: 0)) * 10000 rq: 1 / 99.
r := GVerb in: q roomsize: 99 revtime: 9 damping: 0.2 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.
((HPF in: r + p freq: 50) clump: 2) mix * 0.2
