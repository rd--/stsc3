(* tw 0028 (f0) ; http://www.fredrikolofsson.com/f0blog/?q=node/537 ; requires=kr ; wait for it... *)
var p = Dseq(inf, Dshuf(8, [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987] % 8 * 99));
var q = CombN(Demand(Impulse(8, 0), 0, p), 4, 4, 16);
var o = LfTri(q, 0) / 4;
var f = LfTri(1/16, 0) * 2000 + 3000;
MoogFf(o, f.kr, 2, 0) ! 2
