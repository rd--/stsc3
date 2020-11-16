"synthetic piano (jmcc) #3"
|p|
p := [:_i |
 |n f ph s e c l c_|
 n := 36 rand: 90 .
 f := 0.1 rand: 0.5 .
 ph := 0.0 rand: 1.0 .
 s := Impulse freq: f phase: ph mul: 0.1 .
 e := Decay2 in: s attackTime: 0.008 decayTime: 0.04 .
 c := [:i |
  |n0 o dt|
  n0 := LFNoise2 freq: 3000 .
  o := #(-0.05 0.0 0.04) at: i .
  dt := (n + o) midicps reciprocal .
  CombL in: (n0 * e) maxdelaytime: dt delaytime: dt decaytime: 6 ] .
 l := ((n - 36) / 27) - 1 .
 c_ := 3 mixFill: c .
 Pan2 in: c_ pos: l level: 1 ] .
6 mixFill: p
