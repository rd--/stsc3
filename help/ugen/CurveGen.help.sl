;; CurveGen
var env = CurveGen(1, [0.5, 2], [9], [-4]);
SinOsc(440 * env, 0) * 0.1
