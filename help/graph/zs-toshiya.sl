;; https://github.com/schollz/18000/blob/main/drone/toshiya.scd ; requires=kr
var hz = 440;
var amp = 1.0;
var note = hz.cpsMidi;
var gen = {
	var sig = SinOsc((note + TChoose(Impulse(Rand(1 / 30, 1 / 5), 0), [0, 9, 4, 14, 5, 2, 17]).kr).MidiCps, 0);
	sig := LPF(sig, LinExp(SinOsc(Rand(1 / 30, 1 / 10), Rand(0, 2 * pi)), -1, 1, 20, 12000)) * 2;
	sig := DelayC(sig, Rand(0.01, 0.03), (LFNoise1(Rand(5, 10)) * 0.01 + 0.02) / NRand(10, 20, 3) );
	Pan2(sig, Lag(LFNoise0(1 / 3), 3), 1) / 12 * amp
};
var osc = Pan2(SinOsc((note - 12).MidiCps, LinLin(LFTri(0.5, 0), -1, 1, 0.2, 0.8)) / 12 * amp, SinOsc(0.1, 0) * 0.2, 1) + gen.dup(12).sum;
var snd = MoogLadder(osc.tanh, LinExp(Lag(LFNoise0(1 / 6), 6), -1, 1, hz * 2, hz * 10), 0);
var z = DelayN(snd, 0.048, 0.048);
var y = { CombL(z, 0.1, LFNoise1(Rand(0, 0.1)) * 0.04 + 0.05, 15) }.dup(7).sum;
4.timesRepeat { y := AllpassN(y, 0.050, [Rand(0, 0.050), Rand(0, 0.050)], 1) };
snd := snd + (LinLin(Lag(LFNoise0(1 / 10), 10), -1, 1, 0.01, 0.06) * y);
snd := snd + [
	Amplitude(snd, 0.01, 0.01).kr,
	LinLin(Lag(LFNoise0(1 / 1), 1), -1, 1, 0, 1),
	RingzBank({ PinkNoise() * 0.007 } ! 2, [hz, hz * 2 + 23, hz * 4 + 53, hz * 8 + 23], [1, 1, 1, 1], [1, 1, 1, 1])
].product;
snd := LPF(snd, 10000);
snd := HPF(snd.tanh, 20);
snd * -10.dbAmp
