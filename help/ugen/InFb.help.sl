(* InFb ; assumes graph is writing to bus zero *)
var input = InFb(1, 0);
SinOsc(input * 1300 + 300, 0) * 0.4
