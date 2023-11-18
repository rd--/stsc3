(* CombL ; array expansion (interior duplication) *)
var n = 8;
EqPan2(
	CombL(
		{ Dust(1) * 0.3 } ! n,
		0.01,
		{ Rand(0.0040, 0.0043) } ! n,
		4
	),
	{ Rand(-1, 1) } ! n
).Mix

(* CombL ; exterior duplication *)
var n = 8;
{
	EqPan(
		CombL(
			Dust(1) * 0.3,
			0.01,
			Rand(0.0040, 0.0043),
			4
		),
		Rand(-1, 1)
	)
} !+ n

(* CombC ; https://sccode.org/1-5fc ; requires=AudioIn ; warning=feedback *)
var reverb = { :input |
	var c = CombL(input, 0.1, SinOsc(0.01, 0) * 0.03 + 0.07, 5) * 0.7;
	XFade2(Lpf(c, 4800), input, -0.5, 0.3)
};
reverb(AudioIn([1, 2]) * 0.1)

(* CombL ; simplistic karplus-strong synthesis (adc) *)
var freq = 440;
var repeatFreq = 0.3;
var exciter = Decay(Impulse(repeatFreq, 0), 0.01) * PinkNoise();
var string = CombL(exciter, 0.1, 1 / freq, 3);
[string, LeakDc(string, 0.995)]

(* CombL ; karplus-strong ; mouse control (adc) *)
var freq = MouseX(220, 1760, 1, 0.2);
var repeatFreq = 0.3;
var exciter = Decay(Impulse(repeatFreq, 0), 0.02) * PinkNoise();
var string = CombL(exciter, 0.1, 1 / freq, 3);
[string, LeakDc(string, 0.995)]

(* CombL ; karplus-strong ; very small frequency range ; note changes in sound quality of the decay (adc) *)
var freq = MouseX(220, 1760, 1, 0.2);
var delayTime = MouseX(1 / 100, 1 / (100 + 2), 0, 0.1);
var repeatFreq = 0.3;
var exciter = Decay(Impulse(repeatFreq, 0), 0.02) * PinkNoise();
var string = CombL(exciter, 0.1, delayTime, 3);
[string, LeakDc(string, 0.995)]

(* ---- CombL ; simplistic karplus-strong synthesis (adc) ; keywords *)
var freq = 440;
var repeatFreq = 0.3;
var exciter = Decay(
	in: Impulse(
		freq: repeatFreq,
		phase: 0
	),
	decayTime: 0.01
) * PinkNoise();
var string = CombL(
	in: exciter,
	maxdelaytime: 0.1,
	delaytime: 1 / freq,
	decaytime: 3
);
[
	string,
	LeakDc(
		in: string,
		coef: 0.995
	)
]
