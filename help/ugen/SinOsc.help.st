"SinOsc ; fixed frequency (hz) and initial-phase (radians)"
SinOsc freq: (69 midicps) phase: 0 mul: 0.1

"SinOsc ; modulate freq"
SinOsc freq: (XLine start: 2000 end: 200 dur: 1 doneAction: 2) phase: 0 mul: 0.1

"SinOsc ; modulate freq"
| f1 f2 |
f1 := XLine start: 1 end: 1000 dur: 9 doneAction: 2.
f2 := SinOsc freq: f1 phase: 0 mul: 200 add: 800. "(-1,1) ; (-200,200) ; (600,1000)"
SinOsc freq: f2 phase: 0 mul: 0.1

"SinOsc ; Modulate phase"
| ph |
ph := SinOsc freq: (XLine start: 20 end: 8000 dur: 10 doneAction: 2) phase: 0 mul: 2 * Float pi.
SinOsc freq: 800 phase: ph mul: 0.1

"SinOsc ; mouse control"
| x y |
x := MouseX minval: 40 maxval: 10000 warp: 1 lag: 0.2.
y := MouseY minval: 0.01 maxval: 0.25 warp: 1 lag: 0.2.
SinOsc freq: x phase: 0 mul: y

"SinOsc ; simple bell-like tone"
| f a o e |
f := #(0.5 1.0 1.19 1.56 2.0 2.51 2.66 3.01 4.1) collect: [ :x | x * 500 ].
a := #(0.25 1.0 0.8 0.5 0.9 0.4 0.3 0.6 0.1).
o := SinOsc freq: f phase: 0 mul: a.
e := EnvPerc attackTime: 0.01 releaseTime: 10 level: 0.1 curve: -4.
o mix * (e asEnvGen: 1 withDoneAction: 2)

"SinOsc ; When two pure tones of slightly different frequency are superposed, our ears
perceive audible beats at a rate given by the difference of the two frequencies."
| f0 f1 d i |
f0 := 220.
f1 := 221.25.
d := (f1 - f0) abs.
i := Impulse freq: d phase: 0 mul: ((SinOsc freq: 0.05 phase: 0 mul: 0.1) max: 0).
SinOsc freq: {f0. f1} phase: 0 mul: 0.1 add: i

"sinOsc ; When two tones are sounded together, a tone of lower frequency is
frequently heard. Such a tone is called a combination tone.  The most
commonly heard combination tone occurs at a frequency f2 - f1."
| f1 f2 f3 a3 |
f1 := 300.
f2 := 300 * 3 / 2. "450"
f3 := (f2 - f1) abs. "150"
a3 := (SinOsc freq: 0.05 phase: 0 mul: 0.1) max: 0.
(SinOsc freq: {f1. f2. f3} phase: 0 mul: {0.1. 0.1. a3}) mix

"sinOsc ; with frequency of zero, operates as table lookup variant of sin"
| mk_phasor ph o1 o2 o3 |
mk_phasor := [ :l :r :f | Phasor trig: 0 rate: (SampleDur new * (r - l) * f) ir start: l end: r resetPos: l ].
ph := mk_phasor value: 0 value: Float pi * 2 value: 440.
o1 := SinOsc freq: 440 phase: 0.
o2 := SinOsc freq: 0 phase: ph.
o3 := ph sin.
{o1. XFade2 inA: o2 inB: o3 pan: (LFTri freq: 0.1 iphase: 0) kr level: 1} * 0.1

"sinOsc ; 15.5 khz"
Pan2 in: (SinOsc freq: 15500 phase: 0) pos: 0 level: 0.75

"sinOsc ; event control"
| f |
f := [ :e |
 Pan2 in: (SinOsc freq: (e x * 24 + 48) midicps phase: 0) pos: e o * 2 - 1 level: e z * e w * 0.5 ].
(Voicer numVoices: 16 voiceFunc: f) mix

"sinOsc ; gliss"
| t l m a f |
t := 60.
l := [ :x | XLine start: (x at: 1) end: (x at: 2) dur: t doneAction: 0 ].
m := #((0.5 0.5) (0.5 1) (1 1.19) (1.19 1.56) (1.56 2) (2.51 2) (3.01 2.66) (4.1 3.01) (4.1 4.1)).
a := #(0.25 0.25 1 0.8 0.5 0.9 0.4 0.3 0.6 0.1 0.1).
f := m collect: l.
(SinOsc freq: f * 500 phase: 0 mul: a * 0.05) splay

"----"
o := SinOsc freq: 440 phase: 0
o hash
o freq
o perform: #freq
o paramNames collect: [ :x | o perform: x ]
o class == SinOsc
o class name == #SinOsc
o ugenRate == #ar
