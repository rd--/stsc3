(* Sine ; fixed duration envelope generator ; resets on trigger *)
SinOsc(220, 0) * Sine(Impulse(0.5, 0), 1.5) * 0.1

(* Sine ; envelope generator ; random duration, frequency & amplitude *)
var tr = Impulse(1, 0);
var freq = TRand(220, 330, tr);
var dur = TRand(0.05, 0.95, tr);
var amp = TRand(0.01, 0.2, tr);
SinOsc(freq, 0) * Sine(tr, dur) * amp

(* Sine ; mouse control *)
var f = MouseX(0.25, 8, 0, 0.2);
var a = MouseY(0.05, 0.1, 0, 0.2);
var tr = Impulse(f, 0);
SinOsc(TRand(220, 440, tr), 0) * Sine(tr, 1 / f) * a

(* Sine ; texture *)
{ :tr |
	var freq = { TiRand(5, 23, tr) } ! 2 * 17;
	var dur = { TiRand(3, 7, tr) } ! 2;
	SinOsc(freq, 0) * Sine(tr, dur) / 7
}.OverlapTexture(3, 5, 8).Mix
