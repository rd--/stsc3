(* Sine ; envelope generator ; resets on trigger *)
SinOsc(220, 0) * Sine(Impulse(0.5, 0), 1.5) * 0.1

(* Sine ; mouse control *)
var f = MouseX(0.25, 8, 0, 0.2);
var a = MouseY(0.05, 0.1, 0, 0.2);
var t = Impulse(f, 0);
SinOsc(TRand(220, 440, t), 0) * Sine(t, 1 / f) * a

(* Sine ; texture *)
{ :tr |
	SinOsc({ TiRand(5, 23, tr) } ! 2 * 17, 0) * Sine(tr, { TiRand(3, 7, tr) } ! 2) / 7
}.OverlapTexture(3, 5, 7)
