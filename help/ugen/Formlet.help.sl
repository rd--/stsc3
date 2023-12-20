(* Formlet ; Impulse *)
Formlet(Impulse(20, 0.5), 1000, 0.01, 0.1)

(* Formlet ; Blip ; modulate frequency *)
Formlet(Blip(XLine(10, 400, 8), 1000) * 0.2, 1000, 0.01, 0.1)

(* Formlet *)
Formlet (Blip(XLine([10, 15], [400, 700], 8), 1000) * 0.2, 1000, 0.01, 0.1)

(* Formlet ; mouse control of frequency and decay time *)
var f = LinExp(LfNoise2([1, 3]), -1, 1, [10, 15], [400, 700]);
var s = Blip(f, 1000) * 0.1;
var x = MouseX(0.01, 0.2, 1, 0.2);
var y = MouseY([700, 1300], [2000, 100], 1, 0.2);
Formlet(s, y, 0.005, x)

(* Formlet ; modulate Formant frequency *)
Formlet(Blip(MulAdd(SinOsc(5, 0), 20, 300), 1000) * 0.1, XLine(1500, 700, 8), 0.005, 0.04)

(* Formlet ; noise control *)
var sig = Blip(SinOsc(5, 0) * 20 + 300, 1000) * 0.1;
var frq = LinExp(LfNoise2([0.5, 2.5]), -1, 1, [700, 1300], [2000, 100]);
var dcy = LinExp(LfNoise2([3, 9]), -1, 1, 0.01, 0.2);
Formlet(sig, frq, 0.005, dcy)

(* Formlet ; bass percussion *)
var tr = Impulse(1, 0.5);
Formlet(tr, TRand(30, 50, tr), TRand(0.01, 0.2, tr), 2)

(* Formlet ; parameters randomised on trigger ; stereo *)
var tr = Impulse(LfNoise2([0.15, 0.6]) * 15 + 15, 0.5);
Formlet(
	tr,
	TRand(30, [150, 600], tr),
	TRand(0.01, [0.15, 0.6], tr),
	TRand(0.05, [0.15, 0.6], tr)
) * 0.25

(* Formlet *)
var amp = LfPulse(0.5, 0, 0.5);
var my = MouseY(400, 3200, 0, 0.2);
{
	var x = Formlet({ Dust(12) } ! 2 * 0.05 * amp, my * ExpRand(0.5, 2), 0.005, 0.1);
	AllpassN(x, 0.05, 0.05.Rand, 8)
} !+ 8
