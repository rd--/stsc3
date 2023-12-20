(* 20060909 ; rd *)
var p = {
	var x = MouseX(0.001, 0.02, 1, 0.1);
	var y = MouseY(120, 400, 1, 0.1);
	var f = LfNoise0(4) * [32, 64];
	var w = LfNoise0(32) * x;
	var z = LfNoise0(2) * 0.1;
	var m = LfNoise0(6);
	var s = Pulse(f, w);
	Resonz(s, y + z, (m * 0.4) + 0.8) * 0.5
};
var q = {
	var n = LfNoise0(128);
	CombN(p(), 0.2, (n * 0.1) + 0.1, 3)
};
var r = {
	var x = MouseX(0.75, 1.25, 1, 0.1);
	var y = MouseY(0.25, 1, 1, 0.1);
	{
		{
			var fr = Rand(50, 59) * x;
			var am = Rand(0.04, 0.16) * y;
			SinOsc(fr, 0) * am
		} !+ 16
	} ! 2
};
p() + q() + r()

(* 20060911 ; rd *)
var t = Impulse(22, 0) * (SinOsc(0.5, 0) + 1);
var x = MouseX(0.005, 0.12, 1, 0.1);
var y = MouseY(0.01, 0.52, 1, 0.1);
var z = {
	var n1 = LfNoise0(2);
	var n2 = CoinGate(0.05 + n1 + (y * 0.4) + (t * 0.5), t * 0.5);
	var n3 = TExpRand([500, 900], 1600, t);
	Ringz(n2, n3, x)
} !> 3;
z.Clip2(TRand(0, 1, Dust(8))) * 0.25

(* 20060914 ; rd ; graph rewrite ; requires=Sine ; requires=arrayedEnv *)
{ :tr |
	var chrd = { :m |
		var ds = 3;
		var du = [5, 4, 5, 7, 4, 5];
		var d = du * ds;
		var freq = TxLine(m, m + TRand(0.05, 0.5, tr), d, tr).MidiCps;
		var env = Sine(tr, du.max * ds) * TRand(0.005, 0.01, tr);
		var pos = TxLine(TRand(-1, 1, tr), TRand(-1, 1, tr), d, tr);
		var osc = SinOsc(freq, 0);
		EqPan2(osc, pos).Sum * env
	};
	var scale = [0, 2, 4, 5, 7, 9, 11];
	var octaves = [4, 5, 6, 7];
	var mnn = scale.collect { :n | octaves.collect { :o | n + (o * 12) } }.concatenation;
	var chd = { Choose(tr, mnn) } ! 6;
	{ chrd(chd) } !+ 7
}.OverlapTexture(21, 0, 3).Mix

(* 20060916 ; rd *)
var mkRead = { :l :t |
	BufRd(1, l.asLocalBuf, TRand(0, 6, t), 0, 1)
};
(1 .. 4).collect { :n |
	var t = Dust(1.6);
	var f = mkRead([60, 62, 64, 65, 67, 69], t).MidiCps;
	var p = mkRead([-1, -0.5, 0, 0.25, 0.75, 1], t);
	var a = mkRead([0.01, 0.05, 0.1, 0.15, 0.25, 0.35], t);
	EqPan2(SinOsc(f, 0) * a, p)
}.Mix * 0.25

(* 20060917 ; rd ; requires=DustRange *)
var b0 = [60 71 89 65 36 57 92 97 92 97].asLocalBuf;
var b1 = [71 89 60 57 65 36 95 92 93 97].asLocalBuf;
var clk = DustRange(0.2, 0.9);
var env = Decay2(clk, 0.02, 2.5);
var idx = Stepper(clk, 0, 0, 15, 1, 0);
var f1 = (BufRd(1, [b0, b1], idx, 1, 1) - 24).MidiCps;
var f2 = LfNoise0([1, 3]) * 1.2 + f1;
var o1 = SinOsc(f1, 0) * env;
var o2 = SinOsc(f2, 0) * env;
o1 + o2 * 0.2

(* 2006-09-17 ; rd ; two mono buffers *)
var b0 = [60 71 89 65 36 57 92 97 92 97].asLocalBuf;
var b1 = [71 89 60 57 65 36 95 92 93 97].asLocalBuf;
var clk = DustRange(0.2, 0.9);
var env = Decay2(clk, 0.2, 2.5);
var idx = Stepper(clk, 0, 0, 15, 1, 0);
var f1 = (BufRd(1, [b0, b1], idx, 1, 1) - 24).MidiCps;
var f2 = LfNoise0([1, 3]) * 1.2 + f1;
SinOsc(f1, 0) + SinOsc(f2, 0) * env * 0.2

(* 20060919 ; rd *)
var fw = { :r |
	var t = Dust(3);
	var r1 = TiRand(0, 6, t);
	var r2 = TRand(-0.0001, 0.0001, t);
	var b0 = [
		40 47 42 40 50
		43 35 43 40 47
		45 35 43 42 59
		48 40 47 52 45
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
fw(24) + fw(36)

(* 20060920 ; rd *)
var x = MouseX(0.012, 0.19, 1, 0.1) + (LfNoise2(0.2) * 0.1 + 0.05);
var f = Formlet(Blip(10, 12), LfNoise0([20, 40]) * 43 + 700, 0.005, x);
var o = SinOsc(40, 0) * LfNoise0([5, 10]);
f + o * Line(0, 0.25, 2.5)

(* 20060922 ; rd ; requires=Perc *)
var t0 = Impulse(1 / 0.30, 0);
var t1 = TDelay(t0, 0.15);
var t = [t0, t1];
var k = TRand(56, 57, t);
var i = TRand(40, 480, t);
var j = TRand(-1, 1, t);
var c = k.MidiCps;
var m = (k + 1 + j).MidiCps;
var e = Perc(t, 0.01, 0.9, [-4, -4]);
var f = SinOsc(c, 0) * i + m;
SinOsc(f, 0) * e * 0.1

(* 20060922 ; rd ; requires=Perc *)
var t0 = Impulse(1 / 0.30, 0);
var t = [t0, TDelay(t0, 0.15)];
var k = TRand(56, 57, t);
var m = (k + 1 + TRand(-1, 1, t)).MidiCps;
var f = SinOsc(k.MidiCps, 0) * TRand(40, 480, t) + m;
SinOsc(f, 0) * Perc(t, 0.01, 0.9, [-4, -4]) * 0.1

(* 20060925 ; rd *)
var b = BufAlloc(1, 2048);
var x = MouseX(100, 12000, 0, 0.1);
var y = MouseY(0.01, 0.15, 0, 0.1);
var t = Impulse((LfNoise0([3, 3.25]) * 16) + 18, 0);
var e = Decay2(t, 0.01, TRand(0.005, y, t));
var o = Bpf(WhiteNoise() * e, TRand(10, x, t), TRand(0, 1, t));
var p = PvRandComb(Fft(b, o, 0.5, 0, 1, 0), TExpRand(0.15, 1, t), t);
(o * 0.05) + Ifft(p, 0, 0)

(* 20060927 ; rd ; requires=kr *)
var e = Decay2(Impulse({ Rand(10, 13) } ! 2, 0), 0.001, 0.005);
var f = { Rand(4, 7) } ! 2 * SinOsc({ Rand(10, 13) } ! 2, 0) * e;
var r4 = { TRand(2220, 2227, Impulse(0.7, 0)) } ! 2;
SinOsc(r4.kr, 0) * f.kr * 0.15

(* 20061008 ; rd *)
var x = MouseX(15, 0, 0, 0.1);
var y = MouseY(15, 27, 0, 0.1);
var t = Dust(9).kr;
var b = Choose(t, [36, 48, 60, 72]);
var n = LfNoise1([3, 3.05]) * 0.04;
var d = TiRand(x, y, t);
var e = Decay2(t, 0.005, TRand(0.02, 0.15, t));
var k = DegreeToKey([0, 2, 3.2, 5, 7, 9, 10].asLocalBuf, d, 12);
var f = (b + k + n).MidiCps;
var m = e * SinOsc(f, 0) * 0.2;
var u = PulseDivider(t, 9, 0);
var r0 = TRand(0.0075, 0.125, u);
var r1 = TRand(0.05, 0.15, u);
m * 0.5 + AllpassC(m, 0.15, r0, r1)

(* 20061008 ; rd ; requires=kr *)
var t = Dust(9).kr;
var u = PulseDivider(t, 9, 0);
var d = TiRand(MouseX(15, 0, 0, 0.1), MouseY(15, 27, 0, 0.1), t);
var k = DegreeToKey([0, 2, 3.2, 5, 7, 9, 10].asLocalBuf, d, 12);
var m = LfNoise1([3, 3.05]) * 0.04 + Choose(t, [36, 48, 60, 72]) + k;
var o = SinOsc(m.MidiCps, 0) * Decay2(t, 0.005, TRand(0.02, 0.15, t)) * 0.2;
o * 0.5 + AllpassC(o, 0.15, TRand(0.0075, 0.125, u), TRand(0.05, 0.15, u))

(* 20061017 ; rd *)
var o = SinOsc(LfNoise0([0.5, 1.5]), 0);
var t = Impulse(Slope(o).Abs * [2, 3], 0);
var x = MouseX(960, 3620, 1, 0.2);
var y = MouseY(0.5, 2.0, 0, 0.2);
Ringz(Decay2(t, 0.1, 0.2), TRand(x, 3940, t), TRand(0.005, 0.275, t) * y)

(* 20061023 ; rd *)
var n1 = LfNoise0([0.5, 1.5]);
var o = SinOsc(n1, 0);
var a = Slope(o).Abs * [2, 3];
var t = Impulse(a, 0);
var i = Decay2(t, 0.1, 0.2);
var x = MouseX(960, 3620, 1, 0.2);
var y = MouseY(0.5, 2.0, 0, 0.2);
var n2 = TRand(x, 3940, t);
var n3 = TRand(0.005, 0.275, t);
Ringz(i, n2, n3 * y)

(* 20061027 ; rd *)
var h0 = {
	var a = LfNoise0(1) * 0.2 + 1.2;
	var b = LfNoise0(1) * 0.15 + 0.15;
	var f = 40;
	var h = HenonN([f, f * 0.5], a, b, 0, 0);
	Saw(h * 3200 + 1600) * 0.35
 };
var h1 = {
	var n0 = LfNoise0(32);
	var n1 = LfNoise0(2);
	var a = MouseX(1.2, 1.4, 0, 0.1);
	var b = MouseY(0.2, 0.3, 0, 0.1);
	var h = n0.Range(1, 32);
	var p = n1.Range(2400, 3200);
	var l = n1.Range(-0.75, 0.75);
	var g = n1.Range(0.55, 0.85);
	var f = 40;
	var o = Blip(HenonN(f, a, b, 0, 0).Range(p, p * 2), h);
	EqPan(o, l) * g * 0.35
 };
h0() + h1()
