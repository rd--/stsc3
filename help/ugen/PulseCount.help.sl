(* PulseCount ; count impulses with periodic reset *)
SinOsc(PulseCount(Impulse(10, 0), Impulse(0.4, 0)) * 200, 0) * 0.05
