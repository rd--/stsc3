;; 20060919 ; rd
var fwalk = { :r |
	var t = Dust(3);
	var r1 = TiRand(0, 6, t);
	var r2 = TRand(-0.0001, 0.0001, t);
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
	var f = BufRd(1, [b0, b1], r1, 0, 2);
	var o1 = Blip((r + f).MidiCps, 12);
	var o2 = Blip((r + f + r2).MidiCps, 12);
        o1 + o2 * Decay2(t, 0.3, 1.2) * 0.1
};
fwalk(24) + fwalk(36)
