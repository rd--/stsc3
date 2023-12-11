(* SinOsc ; phase value = (0, 2 * pi), offset to lowest and midpoint ascending *)
SinOsc(110, 2 * pi * [0.75, 0]) * 0.1

(* SinOsc *)
{
	var tr = Dust(1);
	var freq = TRand(3785, 3800, tr);
	var amp = SinOsc(TRand(10, 30, tr), 0) * 0.2;
	var osc = SinOsc(freq, 0) * amp + SinOsc({ TRand(2.91, 3.02, tr) } ! 2 * freq, 0);
	var env = Decay2(tr, 0.01, 1);
	osc * env * 0.4
} !> 12

(* SinOsc ; two oscillators ; frequencies approach 500 from opposite directions *)
var lfo = MouseX(1, 0, 0, 0.2);
var sig = SinOsc(LinExp(lfo, 0, 1, 100, 500), 0) + SinOsc(LinExp(lfo, 0, 1, 1000, 500), 0);
sig * 0.1

(* SinOsc ; https://scsynth.org/t/5169/5 (dietcv) *)
{ :tr |
	var syncEgTop = TRand(8, 20, tr);
	var syncRatio = 2;
	var syncDcy = TRand(0.5, 2, tr);
	var syncEnv = TxLine(syncEgTop / syncRatio, 1, syncDcy, tr);
	var gainEnv = Adsr(Trig(tr, syncDcy), 0.3, 0.3, 0.5, 0.1, -4);
	var freq = { Choose(tr, [45, 48, 52, 53, 57, 58, 60, 64, 65, 70]).MidiCps } ! 5;
	var in = LfTri(freq, 0);
	var phase = Sweep(in, freq * syncRatio * syncEnv);
	var synced = SinOsc(0, (phase % 1) * 2 * pi).Squared;
	var sig = synced * in * gainEnv;
	sig.Splay2 * 0.2
}.OverlapTexture(2, 0, 2).Mix

(* https://scsynth.org/t/6264/2 *)
var numPartials = 64;
var spectrum = [1 .. numPartials];
var inharmonic = MouseX(0.002, 0.008, 0, 0.2);
var tension = (1 + (spectrum * spectrum * inharmonic)).Sqrt;
var tilt = MouseY(-5, -1, 0, 0.2);
var ampArray = (spectrum.log2 * tilt).DbAmp;
var freq0 = 110;
var freq = freq0 * spectrum * tension;
var sig = SinOsc(freq, { Rand(0, 2 * pi) } ! numPartials);
(sig * ampArray).Sum / numPartials

(* SinOsc ; https://scsynth.org/t/6256/5 ; bipolar version *)
var freq = 130;
var squeezeStretch = LfTri(0.1, 0) * 5;
var tri = LfTri(freq, 1) * 0.5 + 0.5;
var pulse = LfPulse(freq, 0, 0.5) * 2 - 1;
var outPhase = pulse * (tri ^ (2 ^ squeezeStretch));
SinOsc(0, outPhase * pi) * 0.1

(* SinOsc ; https://llllllll.co/t/45623/25 *)
var freqBase = 200;
var freqRes = MouseY(100, 1200, 0, 0.2);
var pdbase = Impulse(freqBase, 0);
var pd = Phasor(pdbase, 2 * pi* freqBase / SampleRate(), 0, 2 * pi, 0);
var pdres = Phasor(pdbase, 2 * pi * freqRes / SampleRate(), 0, 2 * pi, 0);
var pdi = LinLin((2 * pi - pd).Max(0), 0, 2 * pi, 0, 1);
Lag(SinOsc(0, pdres) * pdi, 1 / freqBase)

(* SinOsc ; https://scsynth.org/t/6264/9 (es) *)
var freq = [440, 880];
var k = 12000 * (SampleRate() / 44100) / (freq * freq.log);
var sinSig = SinOsc(freq, 0);
var cosSig = SinOsc(freq, pi / 2);
var sqSig = (sinSig * k).Tanh;
var sawSig = sqSig * (cosSig + 1);
sawSig * 0.1

(* SinOsc ; https://scsynth.org/t/6264/8 (fm) *)
var freq = 110;
var indexLimit = 1.5;
var index = LinLin(SinOsc(1 / 10, 3 * pi / 2), -1, 1, 1, indexLimit);
var phase = index * LocalIn(1, 1);
var sig = SinOsc(freq, phase) + SinOsc(freq * 0.001, index * LocalIn(1, 1)).SoftClip;
sig <! LocalOut(sig) * 0.1

(* SinOsc *)
SinOsc(440, 0) * 0.1

(* SinOsc ; used as both oscillator and lfo *)
var f = SinOsc(4, 0);
SinOsc(f * 200 + 400, 0) * 0.1

(* SinOsc ; cancellation (silence) *)
var o1 = SinOsc(440, 0);
var o2 = SinOsc(440, pi);
o1 + o2

(* SinOsc ; modulate freq *)
var f1 = XLine(1, 1000, 9);
var f2 = SinOsc(f1, 0) * 200 + 800;
SinOsc(f2, 0) * 0.25

(* SinOsc ; modulate phase *)
var ph = SinOsc(XLine(20, 8000, 10), 0) * 2 * pi;
SinOsc(800, ph) * 0.1

(* SinOsc ; phase input only *)
var ph = SinOsc(XLine(20, 8000, 10), 0) * 2 * pi;
SinOsc(0, ph) * 0.1

(* SinOsc ; multiple channel expansion *)
SinOsc([220, 221], 0) * 0.1

(* SinOsc ; multiple channel expansion ; named variable *)
var freq = [220 221];
SinOsc(freq, 0) * 0.1

(* SinOsc ; mix to two channel *)
SinOsc([220 221 440 441], 0).Splay2 * 0.1

(* SinOsc ; nested mce *)
SinOsc([220 221; 440 441], 0).Mix * 0.1

(* SinOsc ; http://earslap.com/article/combination-tones-and-the-nonlinearities-of-the-human-ear.html *)
var freqSweep = Line(4000, 1000, 10);
SinOsc([freqSweep, freqSweep + 400], 0).mean

(* SinOsc *)
SinOsc(
	LfNoise2(
		SinOsc([3, 5], 0).Range([2, 7], 35)
	).Range(100, [200, 300]),
	0
) * 0.1

(* SinOsc ; simple fm *)
var modFreq = [400, MouseX(400, 700, 1, 0.2)];
var modDev = SinOsc(LfNoise2(1) + 1, 0).Abs * 1200;
var modOsc = SinOsc(modFreq, 0) * modDev;
var carFreq = [400, MouseY(400, 500, 1, 0.2)];
SinOsc(carFreq + modOsc, 0) * LfNoise2([1, 2]).Range(0, 0.1)

(* SinOsc ; simple fm *)
var modFreq = MouseX(1, 1000, 1, 0.2);
var modDev = MouseY(0, [100, 700], 0, 0.2);
var modOsc = SinOsc(modFreq, 0) * modDev;
var carFreq = 400;
var carAmp = 0.1;
SinOsc(carFreq + modOsc, 0) * carAmp

(* SinOsc ; simple fm ; modIndex *)
var modFreq = MouseX(1, 1000, 1, 0.2);
var modIndex = MouseY(0, [7, 11], 0, 0.2);
var modDev = modFreq * modIndex;
var modOsc = SinOsc(modFreq, 0) * modDev;
var carFreq = 440;
var carAmp = 0.1;
SinOsc(carFreq + modOsc, 0) * carAmp

(* SinOsc ; simple fm ; modRatio & carRatio *)
var freq = 440;
var modRatio = MouseX(0.1, 10, 1, 0.2);
var modDev = MouseY(0, 100, 0, 0.2);
var modOsc = SinOsc(freq * modRatio, 0) * modDev;
var carRatio = [1, 1 / 3] / modRatio;
var carAmp = 0.1;
SinOsc(freq * carRatio + modOsc, 0) * carAmp

(* SinOsc ; SuperCollider derived temporaries syntax with initialiser expressions *)
var freq = 440, phase = 0, amp = 0.1;
SinOsc(freq, phase) * amp

(* SinOsc ; Smalltalk derived temporaries syntax with initialiser expressions *)
| freq = 440, phase = 0, amp = 0.1; |
SinOsc(freq, phase) * amp

(* SinOsc ; requires 0.Max(aUgen) *)
var n = 16;
{
	var amp = 0.Max(SinOsc(ExpRand(0.1, 1), Rand(0, 2 * pi))) / n / 2;
	EqPan(
		SinOsc(ExpRand(100, 1000), 0) * amp,
		Rand(-1, 1)
	)
} !+ n

(* ---- SinOsc ; mce ; two out ; each single channel ; hence mono *)
SinOsc([[440], [441]], 0).Sum * 0.1

(* ---- SinOsc ; fm ; https://github.com/Thrifleganger/VisualSynth ; requires=Voicer *)
var fmSignal = Voicer(16) { :e |
	var index = e.y * 3;
	var pitch = e.x * 24 + 48;
	var ratio = 3.5;
	var amp = 0.5;
	var gate = e.w;
	var pan = e.i * 2 - 1;
	var tone = 1;
	var attack = 0.01;
	var sustain = 0.5;
	var release = 1;
	var decay = 0.3;
	var duration = 1;
	var volModDepth = 0;
	var volModRate = 2;
	var pitchModDepth = 0;
	var pitchModRate = 2;
	var panModDepth = 0;
	var panModRate = 2;
	var indexEnv = Line(gate, index, 0.2, duration);
	var volEnv = Adsr(gate, attack, decay, sustain, release, -4);
	var volMod = SinOsc(volModRate, 0) * volModDepth / 2 + 0.5;
	var panMod = SinOsc(panModRate, 0) * panModDepth;
	var pitchMod = SinOsc(pitchModRate, 0) * pitchModDepth;
	var mod = SinOsc(pitch * ratio, 0) * pitch * indexEnv;
	var car = SinOsc(pitch * (2 ^ (pitchMod / 1200)) + mod, 0) * volEnv * amp;
	var filter = Lpf(car * volMod, LinExp(tone, 0, 1, 200, 20000));
	EqPan2(filter, (panModDepth < 0.01).if { pan } { panMod })
}.Mix;
var delayMix = 0.5, delayTime = 0.3, delayFeedback = 0.3;
var feedbackSignal = LocalIn(2, 0);
var delayReturn = DelayL(fmSignal + feedbackSignal, 5, delayTime);
var delaySignal = (fmSignal + (delayReturn * delayMix)) <! LocalOut(delayReturn * delayFeedback);
var reverbMix = 0.5;
FreeVerb(delaySignal, reverbMix, 0.8, 0.5)

(* alarm *)
var tone = SinOsc([600, 800], 0);
var control = Lpf(LfPulse(2, 0, 0.5), 70);
SelectX(control, tone) * 0.1

(* alarm *)
var tone = SinOsc([723, 932, 1012], 0);
var control = Lpf(Stepper(Impulse(23, 0), 0, 0, 2, 1, 0), 70);
SelectX(control, tone) * 0.1

(* alarm ; mouse selects timbre *)
var freq = Lpf(DmdFor(0.05, 0, Dseq(inf, [723, 932, 1012])), 70);
var osc = SinOsc(freq, 0);
var operations = [osc, (osc * pi).Sin, (osc * pi).Cos, ((osc + 0.25) * pi).Cos];
Select(MouseX(0, 4, 0, 0.2), operations) * 0.1
