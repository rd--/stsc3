;; 20060922 ; fm-iter ; rd ; requires=Perc
var t0 = Impulse(1 / 0.30, 0);
var t = [t0, TDelay(t0, 0.15)];
var k = TRand(56, 57, t);
var m = (k + 1 + TRand(-1, 1, t)).MidiCps;
var f = SinOsc(k.MidiCps, 0) * TRand(40, 480, t) + m;
SinOsc(f, 0) * Perc(t, 0.01, 0.9, [-4, -4]) * 0.1

;; 20060922 ; fm-iter ; rd
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