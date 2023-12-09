(* https://scsynth.org/t/5487 ; mv *)
var prime = LfNoise0(1 / 19) * 8 + 300;
var diff = LfNoise0(1 / 29) + 10;
var fade = 10;
var primetime = 20;
var difftime = 30;
var level= 0.075;
var freqtransition = Lag(prime, primetime);
var left = SinOsc(freqtransition, 0);
var right = SinOsc(freqtransition - Lag(diff, difftime), 0);
var env = Line(0, level, fade);
var brainwave = [left, right] * env;
var tr = Dust(1 / 9);
var freq = TExpRand(285, 310, tr);
var amp = TRand(0.1, 0.2, tr);
var sustain = TRand(7, 14, tr);
var pan = TRand(-0.7, 0.7, tr);
var sig = SinOsc(freq, 0) * amp * Decay2(tr, 0.01, sustain);
brainwave + EqPan(sig, pan)
