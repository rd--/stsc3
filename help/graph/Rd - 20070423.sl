(* 20070423 ; rd ; requires=kr *)
var eggcrate = { :x :y | (x * pi).Cos * (y * pi).Sin };
var p = [64 72 96 128 256 6400 7200 8400 9600];
var x = BrownNoise();
var y = BrownNoise();
var t = Dust(2.4);
var f = LinLin(eggcrate(x, y), -1, 1, TChoose(t, p), TChoose(t, p));
var a = LinLin(x, -1, 1, 0, 0.1);
EqPan(SinOsc(f.kr, 0), y.kr) * a.kr
