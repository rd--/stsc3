|n|
n := [ :i|
  |f p b|
  f := 1.9 raisedTo: i / 128 .
  p := 2 mceFill: [: x| PinkNoise ar: #()].
  b := 4 constant raisedTo: (LFNoise2 kr: ({#freq: -> (1.2 raisedTo: i / 16)})) .
  (BPF ar: ({#in: -> p . #freq: -> (b * 300) . #rq: -> 0.15})) * (5 constant raisedTo: (LFNoise2 ar: {#freq: -> f}) / (i + 8) * 20) ] .
Splay input: (15 mceFill: n)
