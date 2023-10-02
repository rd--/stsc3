(* https://twitter.com/redFrik/status/727992849527083009 *)
var b = LfSaw(1 / 3, 0);
var c = [0, 1];
var d = LfSaw(b, 0);
var i = LfSaw(b + 1 ^ d * (99 + c), 0);
var j = LfSaw(b * 99, c);
var k = LfSaw(1 / 32, 0);
var u = LfSaw(d * 40000 % 2000, 0);
var v = LfSaw(6, c) > 0.9 / 2;
i % j % k + (u * v) / 2
