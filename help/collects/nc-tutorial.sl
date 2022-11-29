;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html ; requires=kr
var n = 11;
var f = {
	var freq = Rand(50, 560.3);
	var numcps = Rand(2, 20);
	var knum = MulAdd(SinOsc(ExpRand(0.02, 0.2),  0), numcps / 2,  numcps / 2);
	var osc = Gendy1(Rand(0, 6), Rand(0, 6), Rand(0, 1), Rand(0, 1), freq , freq,  Rand(0, 1),  Rand(0, 1),  numcps,  knum.kr);
	Pan2(osc,  Rand(-1, 1), 0.5 / n.sqrt)
};
Resonz(f.dup(n).sum, MouseX(100,  2000, 0, 0.2), MouseY(0.01,  1.0, 0, 0.2))

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.1 ; sawtooth
var n = 9;
var f = {
	arg i;
	var mult = (-1 ** i) * (0.5 / (i + 1));
	SinOsc(440 * (i + 1), 0) * mult
};
Pan2(0.to(n).collect(f).sum, 0, 1 / n)

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.1 ; square
var n = 9;
var f = {
	arg i;
	var harmonicnumber = 2 * i + 1;
	SinOsc(440 * harmonicnumber, 0) / harmonicnumber
};
Pan2(0.to(n).collect(f).sum, 0, 1 / n)

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.1 ; triangle
var n = 9;
var f = {
	arg i;
	var harmonicnumber = 2 * i + 1;
	var mult = (-1 ** (harmonicnumber - 1 / 2)) * (1 / (harmonicnumber * harmonicnumber));
	SinOsc(440 * harmonicnumber, 0) * mult
};
Pan2(0.to(n).collect(f).sum, 0, 1 / n)

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.1 ; bell spectra
var rat = [0.5, 1, 1.19, 1.56, 2, 2.51, 2.66, 3.01, 4.1];
var amp = [0.25, 1, 0.8, 0.5, 0.9, 0.4, 0.3, 0.6, 0.1] * 0.1;
SinOsc(500 * rat, 0).sum * amp

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.4 ; fm
var modfreq = MouseX(1, 440, 1, 0.2);
var modindex = MouseY(0.0, 10.0, 0, 0.2);
SinOsc(SinOsc(modfreq,0) * modfreq * modindex + 440, 0) * 0.25

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.4 ; fm
var carfreq = 440;
var harmonicity = MouseX(0, 10, 0, 0.2).roundTo(1);
var modindex = MouseY(0.0, 10.0, 0, 0.2);
var modfreq = carfreq * harmonicity;
SinOsc(carfreq + (SinOsc(modfreq, 0) * modfreq * modindex), 0.0) * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.4 ; pm
var modfreq = MouseX(1,1000, 1, 0.2);
var modindex = MouseY(0.0, 100.0, 0, 0.2);
var conversion = 2 * pi / SampleRate();
SinOsc(0, Phasor(0, 440 * conversion, 0, 2 * pi, 0) + (modfreq * modindex * conversion * SinOsc(modfreq, 0))) * 0.25

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.4 ; pm ; equivalent
var modfreq = MouseX(1,1000, 1, 0.2);
var modindex = MouseY(0.0, 100.0, 0, 0.2);
var conversion = 2 * pi / SampleRate();
SinOsc(440, (modfreq * modindex * conversion * SinOsc(modfreq, 0))) * 0.25

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.5 ; chorus
Saw([440, 443 ,437]).sum * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.5 ; am, fm, chorus
var src = Saw([440, 443, 437] + (SinOsc(100, 0) * 100));
var amp = LfSaw(Line(3, 17, 3, 0),0) * 0.5 + 0.5 * Line(1, 0, 10, 0);
Resonz(src, XLine(10000, 10, 10, 0), Line(1, 0.05, 10, 0)).sum * amp

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 2.5 ; bell patch ; requires=kr
var spectrum = [0.5, 1, 1.19, 1.56, 2, 2.51, 2.66, 3.01, 4.1];
var amplitudes = [0.25, 1, 0.8, 0.5, 0.9, 0.4, 0.3, 0.6, 0.1];
var numpartials = spectrum.size;
var modfreqs1 = { Rand(1, 5) } ! numpartials;
var modfreqs2 = { Rand(0.1, 3) } ! numpartials;
var decaytimes = 1.to(numpartials).collect({ arg i; Rand(2.5, 2.5 + (5 * (1.0 - (i - 1 / numpartials)))) });
var partial = {
	arg i;
	var freq = spectrum.at(i) + (SinOsc(modfreqs1.at(i), 0) * 0.005) * 500;
	var amp = 0.1 * Line(1, 0, decaytimes.at(i), 0) * (SinOsc(modfreqs2.at(i), 0) * 0.1 + 0.9 * amplitudes.at(i));
	Pan2(SinOsc(freq.kr, 0), Rand(-1, 1), amp.kr)
};
1.to(numpartials).collect(partial).sum

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
SinOsc(Stepper(Impulse(10, 0), 0, 1, 10, 1, 1) * 100, 0) * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
Saw(Select(Stepper(Impulse(4, 0.1),  0,  0,  7,  1,  0),  [72, 63, 67, 72, 55, 62, 63, 60].midiCps)) *  0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
Saw(Select(Stepper(Impulse(MouseX(1, 40, 0, 0.2), 0.1),  0,  0,  7,  1,  0),  [72, 63, 67, 72, 55, 62, 63, 60].midiCps)) *  0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
var source = SinOsc(10, 0);
PinkNoise() * 0.1 * [source, Trig1(source, 0.001)]

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
SinOsc(300 + (200 * Latch(SinOsc(13.3, 0), Impulse(10, 0))), 0) * 0.2

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4 ; ?
var env = Env([0, 1, 0, 0.5, -0.4], [0.1], ['step'], nil, nil, 0).asArray;
SinOsc(400 * (1 + EnvGen(Impulse(2.5, 0), 1, 0, 1, 0, env)), 0) * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4 ; ?
var env = Env([63, 63, 60, 55, 60], [0.125], ['step'], nil, nil, 0).asArray;
SinOsc(EnvGen(Impulse(2, 0), 1, 0, 1, 0, env).midiCps, 0) * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
var trig = Impulse(3, 0);
var sound = LfPulse(110 * [1, 5 / 2], 0, 0.5).sum * 0.2;
var env = Decay2(trig, 0.02, 0.2);
Pan2(sound * env, 0, 1)

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4 ; ?
var t = MouseY(0, 1, 0, 0.2) * [0, 128, 256, 128] / SampleRate();
var e = Env([0, 0, 1, -1, 0], t, ['lin'], nil, nil, 0).asArray;
EnvGen(Impulse(MouseX(10, 300, 1, 0.2), 0), 1, 0, 1, 0, e) * 0.2

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4 ; portamento/glide
Saw(Lag(Stepper(Impulse(10, 0), 0, 1, 10, 1, 1) * 200, MouseX(0.0, 0.2, 0, 0.2))) * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
Ringz(Saw(LinExp(Lag(LfNoise0(5), 0.1), -1, 1, 100, 2000)) * 0.2, 1000, 0.01) * 0.1

;; nc ; https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 3.4
Ringz(Saw(LinExp(Lag(LfNoise0(5), MouseX(0.01, 0.3, 0, 0.2)), -1, 1, 100, 2000).roundTo(20)) * 0.2, 1000, 0.01) * 0.1