(* DelayN ; phase-shifting ; mouse delay control *)
var p = SinOsc([440, 441], [0, pi]) * 0.1;
var q = DelayN(SinOsc([440, 441], [pi, 0]), ControlDur() * 2, MouseX(0, ControlDur() * 2, 0, 0.2)) * 0.1;
p + q
