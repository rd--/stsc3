"f0 ; https://twitter.com/redFrik/status/1395040511795372038"
| b c l f o p m h q r |
b := #(2 6 3 1).
c := 500.
l := Lseq repeats: Float infinity list: ((SinOscFb freq: b / 24 feedback: 0) rounded + (0 to: 9) * 7 + 29.17) midiCps.
f := DmdFor dur: b reset: (SinOscFb freq: b / 9 feedback: 0) level: l.
o := SinOscFb freq: f feedback: (((SinOscFb freq: b / 13 feedback: 0) max: 0) / 3 * 2).
p := AllpassN in: o / 8 maxdelaytime: 1 delaytime: 0.51 decaytime: 8.
m := ((SinOscFb freq: b / c feedback: 0) max: 0) / c.
h := Hasher in: ((SinOscFb freq: b / 12 feedback: 0) max: 0) mul: m.
q := Bpf in: h freq: (2 ** (SinOscFb freq: b / c * 2 feedback: 0)) * 10000 rq: 1 / 99.
r := GVerb in: q roomsize: 99 revtime: 9 damping: 0.2 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300.
((Hpf in: r + p freq: 50) clump: 2) sum * 0.2
