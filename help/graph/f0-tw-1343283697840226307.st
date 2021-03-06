"f0 <https://twitter.com/redFrik/status/1343283697840226307>"
| b f z t d c p |
b := (2 to: 12) / 12.
f := #(136.1 163.3 181.4 204.1 244.9 272.1 326.5 362.8 408.2 489.8 544.2).
z := ([ WhiteNoise new ] dup: 11) * ((LFTri freq: b iphase: 0) + 1 / 2) + BrownNoise new.
t := LFTri freq: (121 ** (LFTri freq: (2 / 121 * b) iphase: 0)) iphase: 0.
d := (Lag2 in: ((LFTri freq: (1 / 212.1) iphase: 0) <= (1 / 212)) lagTime: (2 / b)) / 12 * 1.2 + 1.2 / f.
c := (LFTri freq: (b / 12) iphase: 0) + 1 / 2.
p := Pluck in: z trig: t maxdelaytime: (2 / 121) delaytime: d decaytime: (1 / 2 * 12 / 1.2) coef: c.
(HPF in: p freq: 12 mul: 1 / 2.1 / 2) splay2

"---- calculations"
((2 to: 12) / 12) asArray = (11 iota + 1 / 12)
