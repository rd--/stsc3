(* Hpf *)
Hpf(Saw(200) * 0.1, SinOsc(XLine(0.7, 300, 20), 0) * 3600 + 4000)

(* Hpf *)
var c = Hpf(LfSaw(5, 0), SinOsc(XLine(0.07, 30, 20), 0) * 35 + 40);
SinOsc(c * 200 + 500, 0) * 0.1

(* Hpf *)
var c = Hpf(LfSaw(5, 0.1), MouseX(2, 200, 1, 0.2));
SinOsc(c * 200 + 400, 0) * 0.1

(* Hpf *)
var n = Hpf(PinkNoise(), [3000 11000]);
Pan2(n, SinOsc([1 / 7, 1 / 13], [0, pi]), 0.1).Mix
