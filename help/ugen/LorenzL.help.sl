(* LorenzL ; vary frequency *)
LorenzL(MouseX(20, SampleRate(), 0, 0.2), 10, 28, 2.667, 0.05, 0.1, 0, 0) * 0.1

(* LorenzL ; randomly modulate params *)
var freq = SampleRate();
var s = LfNoise0(1) * 2 + 10;
var r = LfNoise0(1) * 20 + 38;
var b = LfNoise0(1) * 1.5 + 2;
LorenzL(freq, s, r, b, 0.05, 0.1, 0, 0) * 0.1

(* LorenzL ; as a frequency control *)
SinOsc(Lag(LorenzL(MouseX(1, 200, 0, 0.2), 10, 28, 2.667, 0.05, 0.1, 0, 0), 0.003) * 800 + 900, 0) * 0.1
