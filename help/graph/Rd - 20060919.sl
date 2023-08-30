(* 20060919 ; rd *)
var fw = { :r |
	var t = Dust(3);
	var b0 = [
		40, 47, 42, 40, 50,
		43, 35, 43, 40, 47,
		45, 35, 43, 42, 59,
		48, 40, 47, 52, 45
	].asLocalBuf;
	var b1 = [
		40, 40, 42, 47, 50,
		35, 43, 43, 40, 45,
		42, 35, 48, 47, 43,
		40, 59, 45, 47, 52
	].asLocalBuf;
	var f = BufRd(1, [b0, b1], TiRand(0, 6, t), 0, 2);
	[
		Blip((r + f).MidiCps, 12),
		Blip((r + f + TRand(-0.0001, 0.0001, t)).MidiCps, 12)
	].sum * Decay2(t, 0.3, 1.2) * 0.1
};
fw(24) + fw(36)
