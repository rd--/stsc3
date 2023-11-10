(* Dx7 ; data at local buffer ; event control *)
var voiceData = [
	25 21 98 38 99  0 99  0 36 17 87  2  1 0 0 0 59  0  1 1 8;
	66 69 60 35  0  0 98  0  0  0  4  0  0 0 0 1 90  0  1 0 8;
	25 21 98 38 99  0 99  2 35 15 79  3  1 1 0 0 91  1  0 0 6;
	97 69 60 35 99 91 98  0  0  0  0  0  0 0 0 1 68  0 22 0 7;
	78 63 53 28 99 52 62  3 38 11 80  3  0 0 0 0 77  0  7 0 0;
	98 56 63 23 99 89 74  2  0  4  1  0  0 0 0 0 99  0  1 0 7;
	99 99 99 99 50 50 50 50  3  2  1 29 99 1 0 0  0  1 24
];
var buf = voiceData.concatenation.asLocalBuf;
var voiceFunc = { :e |
	var x0 = Latch(e.x, e.w);
	var dx7 = Dx7(buf, e.w, 0, 0, 0, e.x * 24 + 48, e.z, (8192 * (e.x - x0)), 0, 0, 0);
	Pan2(dx7, e.i * 2 - 1, 1)
};
Voicer(16, voiceFunc).mixByNamedRule('16×2→1×2')

(* Dx7 ; data at local buffer, random notes *)
var voiceData = [
	25 21 98 38 99  0 99  0 36 17 87  2  1 0 0 0 59  0  1 1 8;
	66 69 60 35  0  0 98  0  0  0  4  0  0 0 0 1 90  0  1 0 8;
	25 21 98 38 99  0 99  2 35 15 79  3  1 1 0 0 91  1  0 0 6;
	97 69 60 35 99 91 98  0  0  0  0  0  0 0 0 1 68  0 22 0 7;
	78 63 53 28 99 52 62  3 38 11 80  3  0 0 0 0 77  0  7 0 0;
	98 56 63 23 99 89 74  2  0  4  1  0  0 0 0 0 99  0  1 0 7;
	99 99 99 99 50 50 50 50  3  2  1 29 99 1 0 0  0  1 24
];
var buf = voiceData.concatenation.asLocalBuf;
var voiceFunc = {
	var tr = Dust(1 / 32).kr;
	var dx7 = Dx7(buf, tr, 0, 0, 0, Rand(tr, 48, 72), Rand(tr, 0.1, 0.6), 0, 0, 0, 0);
	EqPan2(dx7, Rand(tr, -1, 1))
};
voiceFunc !+ 32 * 0.25
