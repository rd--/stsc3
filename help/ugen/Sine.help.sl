(* Sine ; fixed duration envelope generator ; resets on trigger *)
SinOsc(220, 0) * Sine(Impulse(0.5, 0), 1.5) * 0.1

(* Sine ; envelope generator ; random duration, frequency & amplitude *)
var tr = Impulse(1, 0);
var freq = Rand(tr, 220, 330);
var dur = Rand(tr, 0.05, 0.95);
var amp = Rand(tr, 0.01, 0.2);
SinOsc(freq, 0) * Sine(tr, dur) * amp

(* Sine ; mouse control *)
var f = MouseX(0.25, 8, 0, 0.2);
var a = MouseY(0.05, 0.1, 0, 0.2);
var t = Impulse(f, 0);
SinOsc(Rand(t, 220, 440), 0) * Sine(t, 1 / f) * a

(* Sine ; texture *)
{ :tr |
	var freq = { IRand(tr, 5, 23) } ! 2 * 17;
	var dur = { IRand(tr, 3, 7) } ! 2;
	SinOsc(freq, 0) * Sine(tr, dur) / 7
}.OverlapTexture(3, 5, 7)
