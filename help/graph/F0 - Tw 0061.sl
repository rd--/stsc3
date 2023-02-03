;; https://fredrikolofsson.com/f0blog/more-sc-twitter/, f0 (0061)
var t = Saw([9, 9.01]);
var f = DmdOn(t, 0, Lseq(inf, [0, 0, 0, 0, 0, 0, 500]));
var p = SinOsc(Lag(DmdOn(t, 0, Lshuf(inf, [0 .. 7] - 1 * 99)), 0.04), 0);
SinOsc(f, p) / 4
