(* Integrator ; out(0) = in(0) + (coef * out(-1)) *)
Integrator(LfPulse(300, 0.2, 0.1), MouseX(0.001, 0.999, 1, 0.2)) * 0.1

(* Integrator ; as envelope *)
Integrator(LfPulse(3, 0.2, 0.0004), MouseX(0.996, 0.999, 1, 0.2)) * FSinOsc(700, 0) * 0.1

(* Integrator ; as envelope *)
var tr = LfPulse(MouseX(3, 21, 1, 0.2), 0.2, 0.0004);
Integrator(tr, TRand(0.996, 0.999, tr)) * FSinOsc(700, 0) * 0.1

(* Integrator ; https://nathan.ho.name/posts/feedback-integrator-networks/ ; blockSize=1 *)
var n = 8;
var fixed = false;
var snd = Impulse(0, 0);
snd := snd + LocalIn(n, 0);
snd := Integrator(snd, 0.99);
snd := { { fixed.if { Rand(-1, 1) } { LfNoise2(pi) } * 1000 } ! n } ! n * snd;
snd := snd.Sum;
snd := LeakDc(snd, 0.995);
snd := snd.Clip2(1);
snd.Splay <! LocalOut(snd) * 0.1
