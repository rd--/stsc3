(* ExpRandN *)
SinOsc(ExpRandN(2, 110, 220), 0) * RandN(2, 0.05, 0.10)

(* ExpRand ; duplicate *)
{ SinOsc(ExpRand(110, 220), 0) * ExpRand(0.05, 0.10) } ! 2
