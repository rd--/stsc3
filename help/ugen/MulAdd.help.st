"mulAdd"
MulAdd in: (SinOsc freq: 440 phase: 0) mul: 0.1 add: 0.05

"mulAdd ; optimise constants ; note: the auto-generator does not write this"
(MulAdd in: (SinOsc freq: 440 phase: 0) mul: 1 add: 0) * 0.1
