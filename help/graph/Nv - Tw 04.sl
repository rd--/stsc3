(* nv ; https://swiki.hfbk-hamburg.de/MusicTechnology/899 ; wait *)
var s = LocalIn(2, 0) * 7.5 + (Saw([32, 33]) * 0.2);
var f = 2 ^ (LfNoise0(4 / 3) * 4) * 300;
var a = CombN(Bpf(s, f, 0.1).Distort, 2, 2, 40);
a <! LocalOut(a)
