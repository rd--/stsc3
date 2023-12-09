(* pentatonic pipes (jmcc) ; mousex on right half of screen causes pulsation ; requires=kr *)
var n = 5;
var mode = [0 3 5 7 10].asLocalBuf;
var root = 36 + 12.atRandom;
var z = { :tr |
	var m = DegreeToKey(mode, TRand(0, 20, tr), 12) + root;
	var o = Resonz(PinkNoise() * 20, m.MidiCps, 0.002) * 4;
	EqPan2(
		o.Distort * 0.2,
		TRand(-1, 1, tr)
	)
}.OverlapTexture(10, 0.1, n).Mix;
z := z * LinXFade2(1, SinOsc(5, 0).Max(0), MouseX(0, 1, 0, 0.2) > 0.5).kr;
CombN(z, 0.3, 0.3, 8) + z.reversed
