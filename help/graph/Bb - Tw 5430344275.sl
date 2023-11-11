(* https://twitter.com/earslap/status/5430344275 *)
var n = { :freq | LfNoise0(freq) };
var f = [60, 61];
var l = n(6);
var o = SinOsc(f * (l * 9).Ceiling.Lag(0.1), 0) * 0.7;
var p = n(4).Max(l).Max(o);
var q = BBandPass(p, f, n(1).Abs / 2);
(q * 700 * l.Lag(1)).Tanh
