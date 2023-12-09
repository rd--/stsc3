(* TLinRand ; n linearly distributed random numbers every second ; note .kr *)
var n = 12;
var tr = Impulse(1, 0).kr;
Splay(SinOsc({ TLinRand(440, 880, 0, tr) } ! n, 0) * 0.1)
