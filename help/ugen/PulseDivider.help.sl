(* PulseDivider *)
var p = Impulse(8, 0);
var d = PulseDivider(p, [4, 7], 0);
var a = SinOsc(1200, 0) * Decay2(p, 0.005, 0.1);
var b = SinOsc(600, 0) * Decay2(d, 0.005, 0.5);
(a + b) * 0.1
