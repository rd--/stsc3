"f0 ; https://twitter.com/redFrik/status/456384156159574016"
| a s f |
a := #(3 12 4 1 6 2) collect: [:x | 1 / x].
s := (Lag3 in: (SinOsc freq: a phase: 0) lagTime: (SinOsc freq: (2.67 constant raisedTo: a) phase: 0) abs) * 99.
f := (((SinOsc freq: (1 constant / a) / 9 phase: a) > 0) * 20 + 99) / a.
(Splay input: (SinOsc freq: (HPF in: (Ringz in: s freq: f decaytime: 1) freq: 440) phase: 0)) * 0.25
