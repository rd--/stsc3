;; Break Point Envelope ; arranged as [l1, t2, l2, t3, l3 ... ln] ; initial time is zero
var env = EnvBreakPoint([0, 5, 1, 6, 0], [4, -4]).asEnvGen(1);
SinOsc(LinLin(env, 0, 1, 220, 880), 0) * env * 0.1
