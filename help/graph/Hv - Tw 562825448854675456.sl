(* https://twitter.com/HernaniVillase/status/562825448854675456 ; requires=kr ; edit (rd) *)
{
	var o = LfTri(DmdFor(0.1, 0, Dseq(inf, [999 99 4000])).kr, 0);
	AllpassC(o * Dust(DmdFor(1, 0, Dseq(inf, [1 5]))).kr, 0.2, 0.02, 1)
} ! 2
