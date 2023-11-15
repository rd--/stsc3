(* https://fredrikolofsson.com/f0blog/more-sc-twitter/, f0 (0061) *)
var t = Saw([9, 9.01]);
var f = Demand(t, 0, Dseq(inf, [0, 0, 0, 0, 0, 0, 500]));
var p = SinOsc(Lag(Demand(t, 0, Dshuf(inf, (0 .. 7) - 1 * 99)), 0.04), 0);
SinOsc(f, p) / 4
