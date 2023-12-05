(* https://twitter.com/redFrik/status/1453520892374441986 ; f0 *)
var b = [1, 2, 3] / 3;
var c = 2 * pi * b;
var t = SinOsc(10, pi * b);
var x = SinOsc(b, 0) > 0 * SinOsc(b * 50, 0) + 1 / 2;
var y = 300 ^ SinOsc(1 / 32, c) + 99 * x;
var z = SinOsc(1 / 256, c).RoundTo(1) * 50 + 99 + y;
var f = Latch(z, t % SinOsc(99 ^ SinOsc(1 / 64, 0), 0));
var g = SinOsc(SinOsc(1 / 16, 0) * 3 + 12, 0) + 1 / 2 * (SinOsc(1 / 8, c) * 50 + 51);
var h = Latch(g, t * SinOsc(1 / 4, 0));
var o = Blip(f, h).Splay / 3;
o + GVerb(o.Sum / 3 / 3, 30, 3, 0.5, 0.5, 15, 1, 0.7, 0.5, 300)
