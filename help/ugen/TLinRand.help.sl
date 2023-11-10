(* TLinRand ; n linearly distributed random numbers every second ; note .kr *)
var n = 12;
var tr = Impulse(1, 0).kr;
Splay2(SinOsc({ LinRand(tr, 440, 880, 0) } ! n, 0) * 0.1)
