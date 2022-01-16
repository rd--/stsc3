"http://sccode.org/1-4Tw (tb)"
| i f n g |
i := (Impulse freq: 1 / #(1 3 5) phase: #(0 0.133 0.5)) mix.
f := [:st :z | {TDuty dur: ((Timer trig: (st at: 1)) max: 0.25) reset: 0 doneAction: 0 level: 1 gapFirst: 0} , st].
n := (1 to: 10) inject: {i} into: f.
g := [:x :j | SinOsc freq: (4000 + (j * 500)) phase: 0 mul: (Decay2 in: x attackTime: 0.01 decayTime: 0.2)].
(n with: (0 to: 10) collect: g) splay2
