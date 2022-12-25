;; https://fredrikolofsson.com/f0blog/more-sc-twitter/ ; ?
var sy = Saw([3, 4]) * 32 + 64;
var sq = Seq(99, [2, 2, 2, 2, 2, 2, 4, 3]) * (4 ** [0 .. 4]);
var sw = Saw([4, 3]) * 99 + DmdFor(1, 0, sq).transpose;
CombN(SyncSaw(sy, sw) / 9, 1, 1 / 6, 2).transpose.sum * 0.1

;; https://fredrikolofsson.com/f0blog/more-sc-twitter/ ; DmdOn variation ; ?
var sy = Saw([3, 4]) * 32 + 64;
var sq = Seq(99, [2, 2, 2, 2, 2, 2, 4, 3]) * (4 ** [0 .. 4]);
var sw = Saw([4, 3]) * 99 + DmdOn(Impulse(1, 0), 0, sq).transpose;
CombN(SyncSaw(sy, sw) / 9, 1, 1 / 6, 2).transpose.sum * 0.1