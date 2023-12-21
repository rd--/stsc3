"<https://twitter.com/redFrik/status/1452318302768963589> ; f0"
| b m c t f h a z o |
b := 2 * Float pi * #(4 3 1 2 5) / 5.
m := 2 ** (SinOsc freq: 1 / 50 phase: b) * 99.
c := m roundTo: 50.
t := b / (2 * Float pi) + ((SinOsc freq: b / 50 phase: (SinOsc freq: ((SinOsc freq: 1 / 99 phase: Float pi) max: 0) phase: 0) ** (2 * Float pi)) max: 0).
f := (SinOsc freq: b / 9 phase: b) / 3 + c.
h := CombC in: c / 50 maxdelaytime: 2 delaytime: t decaytime: 5.
a := 50 / m * ((SinOsc freq: b / 50 phase: 0) max: 0) + 0.5.
z := Blip freq: f numharm: h mul: a.
o := Hpf in: z splay2 * 1.5 freq: 15 tanh.
o + (GVerb in: o sum / 2 * (Line start: 0 end: 1 dur: 1 doneAction: 0) roomsize: 50 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300) / 5
