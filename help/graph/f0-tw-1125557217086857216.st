"https://twitter.com/redFrik/status/1125557217086857216 (f0)"
| scl b i m v o s |
scl := #(0 2.94 4.98 7.02 9.96). "pythagorean"
b := (Interval from: -7 to: 6) asArray + 0.7 * 2 / 666.
i := (LFTri freq: b iphase: b mul: (LFTri freq: b iphase: 0 mul: 9)) + 9 % 32.
m := (DegreeToKey bufnum: (scl asLocalBuf) in: i octave: 12) + 24.
v := VarSaw freq: m midicps iphase: 0 width: (LFTri freq: b iphase: 0) + 1 / 2.
o := v * (AmpComp freq: m root: 440 exp: (1/3)) * (LFTri freq: b iphase: b) * b * 9.
s := RLPF in: o freq: (Lag2 in: m lagTime: (1 constant / b % 1)) * 3 rq: 1.
(Splay input: (AllpassN in: s maxdelaytime: 0.3 delaytime: (b collect: [:x | 0.2 - x]) decaytime: 3)) tanh
