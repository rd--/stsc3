(* TableWindow *)
var tbl = (0 .. 511).collect { :each |
	(each / 512 * pi).sin
}.asLocalBuf;
var k = 7;
var dur = 1 / k;
var tr = TrigRoundRobin(k, Impulse(k, 0));
var win = TableWindow(tr, dur, tbl);
Splay(
	SinOsc(ExpRand(tr, 111, 555), 0) * win / 3
)
