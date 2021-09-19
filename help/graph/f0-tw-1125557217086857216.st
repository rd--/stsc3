"https://twitter.com/redFrik/status/1125557217086857216 (f0)"
| scl b i m v o s |
scl := #(0 2.94 4.98 7.02 9.96). "pythagorean"
b := (-7 to: 6) + 0.7 * 2 / 666.
i := (LFTri freq: b iphase: b mul: (LFTri freq: b iphase: 0 mul: 9)) + 9 % 32.
m := (DegreeToKey bufnum: (scl asLocalBuf) in: i octave: 12) + 24.
v := VarSaw freq: m midicps kr iphase: 0 width: (LFTri freq: b iphase: 0) kr + 1 / 2.
o := v * (AmpComp freq: m root: 440 exp: 1/3) kr * (LFTri freq: b iphase: b) kr * b * 9.
s := RLPF in: o freq: (Lag2 in: m lagTime: (1 / b % 1)) * 3 rq: 1.
(AllpassN in: s maxdelaytime: 0.3 delaytime: (0.2 - b) decaytime: 3) splay tanh
