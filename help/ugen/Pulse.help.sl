(* Pulse *)
Pulse(MouseX(100, 200, 1, 0.2), 0.5) * 0.1

(* Pulse ; https://nathan.ho.name/posts/integer-ring-modulation/ *)
var freq = 100;
Pulse(freq, 0.5) * Pulse(freq * LinLin(LfTri(0.3, pi), -1, 1, 1, 20).RoundTo(1), 0.5) * 0.1

(* Pulse ; interference *)
Pulse([100, 100.1], 0.5).Sum * 0.1

(* Pulse ; envelope *)
SinOsc(234, 0) * Lag((Pulse(MouseX(3.14, 23, 1, 0.2), MouseY(0.23, 0.78, 1, 0.2)) > 0), 0.01) * 0.1
