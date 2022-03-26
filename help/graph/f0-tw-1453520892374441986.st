"https://twitter.com/redFrik/status/1453520892374441986 ; f0"
| b c t x f y h o |
b := #(1 2 3) / 3.
c := 2 * Float pi * b.
t := SinOsc freq: 10 phase: Float pi * b.
x := (SinOsc freq: b phase: 0) > 0 * (SinOsc freq: b * 50 phase: 0) + 1 / 2 * (300 ** (SinOsc freq: 1 / 32 phase: c) + 99) + ((SinOsc freq: 1 / 256 phase: c) rounded * 50 + 99).
f := Latch in: x trig: t % (SinOsc freq: 99 ** (SinOsc freq: 1 / 64 phase: 0) phase: 0).
y := (SinOsc freq: (SinOsc freq: 1 / 16 phase: 0) * 3 + 12 phase: 0) + 1 / 2 * ((SinOsc freq: 1 / 8 phase: c) * 50 + 51).
h := Latch in: y trig: t * (SinOsc freq: 1 / 4 phase: 0).
o := (Blip freq: f numharm: h) splay2 / 3.
o + (GVerb in: o sum / 3 / 3 roomsize: 30 revtime: 3 damping: 0.5 inputbw: 0.5 spread: 15 drylevel: 1 earlyreflevel: 0.7 taillevel: 0.5 maxroomsize: 300)
