(* TGrains ; requires=SfAcquire *)
var sf = SfAcquireMono('crotale-d6');
var trRate = MouseY(2, 120, 1, 0.2);
var tr = Impulse(trRate, 0);
var rate = 1.2 ^ (WhiteNoise() * 3).RoundTo(1);
var centerPos = MouseX(0, SfDur(sf), 0, 0.2);
var dur = 1.2 / trRate;
var nc = 2;
TGrains(nc, tr, sf, rate, centerPos, dur, WhiteNoise(), 0.1, 4)

(* ---- notes.md ---- *)
{ Dc(0) } ! 8 ++ TGrains(8, tr, sf, rate, centerPos, dur, WhiteNoise(), 0.1, 4)
