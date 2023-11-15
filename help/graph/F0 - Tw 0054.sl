(* https://fredrikolofsson.com/f0blog/more-sc-twitter/ *)
var sq = Dseq(99, [2, 2, 2, 2, 2, 2, 4, 3]);
var sy = Saw([3, 4]) * 32 + 64;
var sz = Saw([4, 3]) * 99;
(0 .. 4).collect { :x |
	var sw = sz + DmdFor(1, 0, sq * (4 ^ x));
	CombN(SyncSaw(sy, sw) / 9, 1, 1 / 6, 2) * 0.2
}.Mix
