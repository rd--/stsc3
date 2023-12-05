(* https://sccode.org/1-4Qy ; f0 ; 0251 *)
var b = (1 .. 8);
var h = SyncSaw(b / 64, SyncSaw([4, 8, 14], b / 4) + 1).Max(0);
var o = SyncSaw(99 * b, b * 2.01) % h;
GVerb(Limiter(Hpf(o.Sum, 9), 1, 0.01) / 2, 70, 4, 0.5, 0.5, 15, 1, 0.7, 0.5, 300)
