"Decay2 ; as envelope"
| tr |
tr := Impulse freq: (XLine start: 1 end: 50 dur: 20 doneAction: 2) phase: 0 mul: 0.25.
SinOsc freq: 600 phase: 0 mul: (Decay2 in: tr attackTime: 0.01 decayTime: #(0.2 0.5))
