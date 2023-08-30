(* https://fredrikolofsson.com/f0blog/more-sc-twitter/ f0 ; 0055 *)
var o = Pulse(Pulse([1, 2] / 3, 1 / 9) * 50 + [50, 150], 0.5);
var m = Pulse([3, 4], 1 / 3) + Pulse([2, 3], 1 / 4) / 10 + 0.005;
CombN((Slope(o) * m).Cos / 5, 1, 1 / 6, 2)
