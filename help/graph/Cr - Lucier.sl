(* lucier (cr) ; http://www.listarc.bham.ac.uk/lists/sc-users/msg47539.html *)
var bus = 20; (* These may audible on large multi-channel systems *)
{ :tr |
	var freq = TrRand(tr, 56, 64);
	var blocksize = 1 / ControlRate();
	var mkDt = { :f | 1 / f - blocksize };
	var stringDelay = mkDt(freq);
	var pk1Pos = 0.1;
	var srcPos = 0.3;
	var pk2Pos = 0.9;
	var maxDelay = 1;
	var mkDelay = { :i :r | Lpz1(DelayC(i, maxDelay, r * stringDelay)) };
	var mkAllpass = { :i :r :dt | Lpz1(AllpassC(i, maxDelay, r * stringDelay, dt)) };
	var drv = InFb(1, bus);
	var pk1R = mkDelay(drv, srcPos - pk1Pos);
	var pk1L = mkAllpass(pk1R * -1, pk1Pos * 2, TrRand(tr, 0.001, 0.11));
	var pk2L = mkDelay(pk1L, pk2Pos - pk1Pos) * 0.99;
	var stringL = mkDelay(pk2L, 1 - pk2Pos);
	var pk2R = mkAllpass(stringL * -1, 1 - pk2Pos, 2 + TrRand(tr, 0.001, 0.11)) * 0.99;
	var stringR = mkDelay(pk2R, pk2Pos - srcPos);
	var source = {
		var s = SinOsc(220, 0) * 0.01;
		var a = Amplitude(drv, 0.01, 0.01) * 11;
		var p = Pulse(60 + a, 0.5) * 0.1;
		var f = Rlpf(s + p, 320, 0.05);
		var e = 1 - Amplitude(drv, 0.01, 0.01).Min(1);
		Normalizer(f, 0.7, 0.01) * e
	};
	var lOut = ReplaceOut(bus, source.value * 0.2 + stringR);
	var outL = pk1L + pk1R;
	var outR = pk2L + pk2R;
	bus := bus + 1;
	[outL, outR] <! lOut <! drv
}.OverlapTexture(1, 5, 3) * 0.25

(* ---- notes.md
The OverlapTexture variant cannot use LocalIn & LocalOut since there can only be one pair per graph.
*)
