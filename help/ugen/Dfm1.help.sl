(* Dfm1 ; mouse control *)
Dfm1(PinkNoise() * 0.2, MouseX(80, 5000, 1, 0.2), MouseY(0.1, 1.2, 0, 0.2), 2, 0, 0.0003)

(* Dfm1 *)
var in = Pulse([100, 100.1], 0.5).Sum * 0.4;
var freq = SinOsc(SinOsc(0.3, 0).Range(0.2, 5), 0).Range(80, 2000);
Dfm1(in, freq, 1.1, 2, 0, 0.0003) * 0.2
