"f0 ; https://twitter.com/redFrik/status/1105496695637454848"
| b w s x y z m o f rq |
b := (1 to: 15) / 151.
w := (VarSaw freq: b iphase: b width: 1 / 5.15 mul: 1 add: 1.5) ceil.
s := Select which: ((VarSaw freq: 1 / 15 iphase: 1 / 5 width: b mul: 5) ceil + 5 / 5) array: {51 * 1.5. 51. 151}.
x := Lag in: s lagTime: b.
y := VarSaw freq: b + 5 iphase: 0 width: 0.5.
z := VarSaw freq: b iphase: b width: b mul: b / 5 add: #(1.5 5 1).
m := VarSaw freq: 5 - b iphase: b width: b mul: 5 add: 5.
o := VarSaw freq: (w * x + y * z) kr iphase: b width: (VarSaw freq: b iphase: 0 width: 0.5) kr + 5 / 15 mul: (m > 1.515) kr.
f := 1515 ** ((VarSaw freq: 1 - b / 5 iphase: 0 width: 0.5) / 15 + 1 + b).
rq := (1.5 ** (VarSaw freq: b iphase: 0 width: 0.5)) / 5.
(BLowPass in: o freq: f kr rq: rq kr mul: 1 / 5) splay2
