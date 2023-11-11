(* SoftClip *)
var e = XLine(0.1, 10, 10);
var o = SinOsc(500, 0);
(o * e).SoftClip * 0.25
