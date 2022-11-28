;; Decay ; as envelope
var tr = LfPulse(MouseX(3, 21, 1, 0.2), 0.2, 0.0004);
Decay(tr, { TRand(0.01, 0.35, tr) } ! 2) * SinOsc({ TRand(500, 700, tr) } ! 2, 0) * 0.1
