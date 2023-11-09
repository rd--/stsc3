(* Atan2 *)
var x = 10;
var y = LfSaw(1/6, 0) * 100;
var distance = Hypot(x, y);
var amplitude = 40 / distance.Squared;
var sound = Rlpf(SinOsc(200, 0) * LfPulse(31.3, 0, 0.4), 400, 0.3);
var azimuth = Atan2(y, x);
var loc = Clip2(azimuth / (pi / 2), 1);
EqPan2(
	DelayL(sound, 110 / 344, distance / 344),
	loc
) * amplitude
