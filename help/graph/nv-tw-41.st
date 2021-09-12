"https://swiki.hfbk-hamburg.de/MusicTechnology/899 (nv) [Line 41] ; autotranslate"
|n|
n := [:i|
  |f p b|
  f := 1.9 ** i / 128.
  p := 2 mceFill: [:x| PinkNoise ar].
  b := 4 asUGen ** (LFNoise2 kr: {1.2 ** i / 16}).
  (BPF ar: {p. b * 300. 0.15}) * (5 asUGen ** (LFNoise2 ar: {f}) / (i + 8) * 20)].
Splay input: (15 mceFill: n)
