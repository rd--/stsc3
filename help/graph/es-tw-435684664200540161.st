"https://twitter.com/sluyterrific_sc/status/435684664200540161 (es)"
| f |
f := [ :i |
  | m |
  m := i % (LFNoise2 freq: 0.01 mul: 50 add: 50) + (LFNoise2 freq: 0.1 mul: 10 add: 40).
  SinOsc freq: m midicps phase: 0 mul: (LFNoise2 freq: 1 mul: 0.01 add: 0.01) ].
(99 iota collect: f) splay
