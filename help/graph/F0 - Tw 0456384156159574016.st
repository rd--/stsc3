"https://twitter.com/redFrik/status/456384156159574016 (f0)"
| a s f |
a := 1 / #(3 12 4 1 6 2).
s := (Lag3 in: (SinOsc freq: a phase: 0) lagTime: (SinOsc freq: (2.67 ** a) phase: 0) abs) * 99.
f := (SinOsc freq: (1 / a / 9) phase: a) > 0 * 20 + 99 / a.
(SinOsc freq: (Hpf in: (Ringz in: s freq: f decaytime: 1) freq: 440) phase: 0 mul: 0.25) splay2
