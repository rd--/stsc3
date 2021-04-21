"http://www.fredrikolofsson.com/f0blog/?q=node/537 (f0)"
| s i o w |
s := Sweep trig: (LocalIn numChan: 6 default: 0) rate: 1.
i := Impulse freq: #(1 0.749 6 12 3 4) phase: 0.
o := SinOsc freq: (RunningMax in: s trig: i) reciprocal phase: 0.
w := (Splay input: o spread: 1 level: 1 center: 0 levelComp: true) tanh * 0.05.
MRG lhs: w rhs: (LocalOut channelsArray: o)
