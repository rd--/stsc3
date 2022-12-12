"f0 https://twitter.com/redFrik/status/1338987643057688579"
| b x t d c z p |
b := (8 to: 18) asArray.
x := VarSaw freq: #(18.1 81.8 1.81) + b iphase: 1 width: (VarSaw freq: (b / 81) iphase: 0 width: 0.5).
t := VarSaw freq: (8 / 1.818) iphase: {1. 8/18} width: 0.5.
d := (LinExp in: (VarSaw freq: (b / 181.8) iphase: 0 width: 0.5) srclo: -1 srchi: 1 dstlo: 1.81 / 818 dsthi: 1.8 / 181.8) roundTo: (1.81 / 818).
c := (VarSaw freq: (1 / 8) iphase: (1 / b) width: 0.5) + 8.18 * 1.8.
z := LinLin in: (VarSaw freq: (1 / 81.8) iphase: 0 width: 0.5) srclo: -1 srchi: 1 dstlo: 1.8 ** -1 dsthi: 8.1 / 8.18.
p := (Pluck in: x trig: t maxdelaytime: (1.8 / 181.8) delaytime: d decaytime: c coef: z) * (VarSaw freq: (1.8 / b) iphase: 0 width: 0.5) / 1.8.
Hpf in: p splay2 freq: (18 / 1.81)
