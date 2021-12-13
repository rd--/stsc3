"XLine ; self deleting"
SinOsc freq: (XLine start: 110 end: 220 dur: 5 doneAction: 2) phase: 0 mul: 0.1

"XLine ; self deleting"
SinOsc freq: 440 phase: 0 mul: (XLine start: 0.15 end: 0.001 dur: 1 doneAction: 2).
