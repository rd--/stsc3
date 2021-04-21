"https://twitter.com/redFrik/status/456384156159574016 (f0)"
| a s f o |
a := {1/3. 1/12. 1/4. 1. 1/6. 1/2}.
s := (Lag3 in: (SinOsc freq: a phase: 0) lagTime: (SinOsc freq: (a collect: [:x | 2.67 raisedTo: x]) phase: 0) abs) * 99.
f := (SinOsc freq: (a collect: [:x | 1 / x / 9]) phase: a) > 0 * 20 + 99 / a.
o := SinOsc freq: (HPF in: (Ringz in: s freq: f decaytime: 1) freq: 440) phase: 0.
(Splay input: o spread: 1 level: 1 center: 0 levelComp: true) * 0.25
