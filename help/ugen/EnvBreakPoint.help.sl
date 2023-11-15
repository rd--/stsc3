(* Break Point Envelope ; arranged as [l1, t2, l2, t3, l3 .. ln] ; initial time is zero *)
var env = EnvBreakPoint([0 5 1 6 0], [4 -4]).asEnvGen(1);
SinOsc(LinLin(env, 0, 1, 220, 880), 0) * env * 0.1

(* ---- calculations *)
EnvBreakPoint([0, 1, 1, 3, 0], -4).asArray = [0, 2, -99, -99, 1, 1, 5, -4, 0, 2, 5, -4]
