"https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41]"
| n |
n := [ :i |
  | f p b |
  f := 1.9 ** i / 128.
  p := 2 mceFill: [ :x | PinkNoise new ].
  b := 4 ** (LFNoise2 freq: 1.2 ** i / 16) kr.
  (BPF in: p freq: b * 300 rq: 0.15) * (5 ** (LFNoise2 freq: f) / (i + 8) * 20) ].
(15 mceFill: n) splay

"https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41] ; autotranslate"
| n |
n := [ :i |
  | f p b |
  f := 1.9 ** i / 128.
  p := 2 mceFill: [ :x | PinkNoise ar: {} ].
  b := 4 ** (LFNoise2 kr: {1.2 ** i / 16}).
  (BPF ar: {p. b * 300. 0.15}) * (5 ** (LFNoise2 ar: {f}) / (i + 8) * 20) ].
(15 mceFill: n) splay
