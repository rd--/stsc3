"f0 <https://twitter.com/redFrik/status/1338987643057688579>"
| b x t d c z p |
b := (Interval from: 8 to: 18) asArray.
x := VarSaw freq: #(18.1 81.8 1.81) mce + b mce iphase: 1 width: (VarSaw freq: (b / 81) iphase: 0 width: 0.5).
t := VarSaw freq: (8 / 1.818) iphase: {1. 8/18} width: 0.5.
d := ((VarSaw freq: (b / 181.8) iphase: 0 width: 0.5) inExpRangeFrom: (1.81 / 818) to: (1.8 / 181.8)) round: (1.81 / 818).
c := (VarSaw freq: (1 / 8) iphase: (b collect: [:e | 1 / e]) width: 0.5) + 8.18 * 1.8.
z := (VarSaw freq: (1 / 81.8) iphase: 0 width: 0.5) inRangeFrom: (1.8 raisedTo: (-1)) to: (8.1 / 8.18).
p := (Pluck in: x trig: t maxdelaytime: (1.8 / 181.8) delaytime: d decaytime: c coef: z) * (VarSaw freq: (1.8 constant / b) iphase: 0 width: 0.5) / 1.8.
HPF in: (Splay input: p) freq: (18 / 1.81)
