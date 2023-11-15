(* https://twitter.com/redFrik/status/1138498427241861122 (f0) *)
var f = 9.sqrt;
var b = [f, 9.999];
var d = SinOscFb(b / 99, 0).Max(0);
var t1 = Select2(SinOscFb(SinOscFb(9 / 999, 0), 0) > SinOscFb(9 / 99, 0), f, 9 / b);
var t2 = Select2(SinOscFb(9 / 99, 0) < SinOscFb(99 / 9999, 0), b / 9, f);
var f1 = Lag(9.9 * b * t1 * t2, 0.1);
var s1 = SinOscFb(f1, d) * SinOscFb(b / 9, 0) * d;
var c = PitchShift(s1, 9 / b, 9 / b.sqrt, b / 999, b / 99);
var s2 = GVerb(c * d * d * d, 99, 9, 9 / 999, 0.5, 15, 1, 0.7, 0.5, 300);
var s3 = s2 / 9 + PitchShift(c, f / 9, f / 9, 0, 0);
Hpf(Splay2(s3), 9).transposed.Mix * 0.5
