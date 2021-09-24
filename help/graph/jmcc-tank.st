"tank (jmcc)"
| r_allpass pling bang tank signal |
r_allpass := [ :z | AllpassN in: z maxdelaytime: 0.03 delaytime: [Rand lo: 0.005 hi: 0.02] dup decaytime: 1 ].
pling := [ :j |
  | d f p s1 s2 |
  d := Dust density: 0.2.
  f := ExpRand lo: 300 hi: 2200.
  p := Rand lo: -1 hi: 1.
  s1 := (FSinOsc freq: f iphase: 0) cubed.
  s2 := Decay2 in: d attackTime: 0.1 decayTime: 0.5 mul: s1.
  Pan2 in: s2 pos: p level: 0.1 ].
bang := [
  | d n |
  d := Dust density: 0.01.
  n := BrownNoise new.
  Pan2 in: (Decay2 in: d attackTime: 0.04 decayTime: 0.3 mul: n) pos: 0 level: 1 ] value.
tank := [ :i |
  | l0 l1 l2 l3 l4 l5 l6 l7 |
  l0 := LocalIn numChan: 2 default: 0 mul: 0.98.
  l1 := OnePole in: l0 coef: 0.33.
  l2 := l1. "RotateStereo in: l1 pos: 0.23."
  l3 := AllpassN in: l2 maxdelaytime: 0.05 delaytime: [Rand lo: 0.01 hi: 0.05] dup decaytime: 2.
  l4 := DelayN in: l3 maxdelaytime: 0.3 delaytime: #(0.17 0.23).
  l5 := AllpassN in: l4 maxdelaytime: 0.05 delaytime: [Rand lo: 0.03 hi: 0.15] dup decaytime: 2.
  l6 := LeakDC in: l5 coef: 0.995.
  l7 := l6 + i.
  Mrg lhs: l7 rhs: (LocalOut channelsArray: l7) ].
signal := (12 mixFill: pling) + bang.
(1 to: 4) do: [ :i | signal := r_allpass value: signal ].
tank value: signal
