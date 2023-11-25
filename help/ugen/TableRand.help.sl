(* TableRand *)
var tbl = { :trig :dist :lo :hi |
	TableRand(trig, dist.asRandomTable(128).asLocalBuf).LinLin(0, 1, lo, hi)
};
var x = MouseX(7, [7 * 11, 11 * 23], 1, 0.2);
var tr = Dust(x).kr;
var carFreq = [
	tbl(tr, [0 1 0 0 1 0 0 0 1 0], 55, 555),
	tbl(tr, [1 0 1 1 0 1 1 1 0 1], 55, 555)
];
var modFreq = carFreq * [
	tbl(tr, [1 0 0 1 0 0 1], 1, 23),
	tbl(tr, [0 1 1 0 1 1 0], 1, 23)
].RoundTo(1);
var index = [
	tbl(tr, [0 0 0 1], 1, 23),
	tbl(tr, [1 0 0 0], 1, 23)
];
GrainFm(1, tr, 7 / x, carFreq, modFreq, index, 0, -1, 512).Mix / 23
