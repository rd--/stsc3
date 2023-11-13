(* Decay2 *)
{
	var tr = Dust(1);
	var env = Decay2(
		tr,
		Rand(tr, 0.01, 0.2),
		Rand(tr, 0.2, 1)
	);
	SinOsc(Rand(tr, 110, 330), 0) * env
} !^ 5

(* Decay2 ; c.f. MultiTouchPad help file *)
Voicer(16) { :e |
	var impulseFreq = Choose(e.w, [1 2 3 4 6 8 9]);
	var oscFreq = Rand(e.w, 80, 880);
	var mul = (1 - e.y) * e.w;
	var pan = (e.x * 2) - 1;
	var distort = e.z * 16;
	var decay = e.z * 2;
	var trig = Impulse(impulseFreq, 0);
	var snd = SinOsc(oscFreq, 0) * Decay2(trig, 0.01, 0.2 * decay);
	EqPan2(
		(snd * distort).Tanh / distort,
		pan.Lag(0.1)
	) * mul.Lag(0.1)
}.Mix

(* Decay2 ; c.f. MultiTouchPad help file ; requires=kr *)
{
	var tr = Dust(1 / 9).kr;
	var impulseFreq = Choose(tr, [1 2 3 4 6 8 9]);
	var oscFreq = Rand(tr, 20, 880);
	var distort = Rand(tr, 0.1, 8);
	var env = Decay2(Impulse(impulseFreq, 0), 0.01, 0.2 * Rand(tr, 0.1, 1));
	var snd = SinOsc(oscFreq, 0) * env;
	EqPan2(
		(snd * distort).Tanh / distort,
		Rand(tr, -1, 1)
	) * XLine(Impulse(0, 0) + tr, Rand(tr, 0.2, 0.5), 0.001, Rand(tr, 3, 7))
} !> 16

(* Decay2 *)
var lfo = SinOsc([1, 11], 0);
var tr = Tr1(lfo);
SinOsc([222, 2222], 0) * Decay2(tr, 0.01, [0.5, 0.1]) * [0.2, 0.1]

(* Decay2 *)
{
	var lfo = SinOsc(Rand(1, 13), 0);
	var tr = Tr1(lfo);
	var env = Decay2(tr, 0.01, Rand(0.1, 0.5));
	SinOsc(Rand(1111, 2222), 0) * env * Rand(0.01, 0.1)
} !> 24
