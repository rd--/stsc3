"https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41]"
| n |
n := [ :i |
  | f p b |
  f := 1.9 ** i / 128.
  p := 2 arrayFill: [ :x | PinkNoise new ].
  b := 4 ** (LFNoise2 freq: 1.2 ** i / 16).
  (BPF in: p freq: b kr * 300 rq: 0.15) * (5 ** (LFNoise2 freq: f) / (i + 8) * 20) ].
(15 arrayFill: n) splay
