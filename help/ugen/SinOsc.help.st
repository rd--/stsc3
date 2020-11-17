"SinOsc ; fixed frequency (hz) and initial-phase (radians)"
SinOsc freq: (69 midicps) phase: 0 mul: 0.1

"SinOsc ; modulate freq"
SinOsc freq: (XLine start: 2000 end: 200 dur: 1 doneAction: 1) phase: 0 mul: 0.1

"SinOsc ; modulate freq"
[|f1 f2|
f1 := XLine start: 1 end: 1000 dur: 9 doneAction: 1 .
f2 := SinOsc freq: f1 phase: 0 mul: 200 add: 800 . "(-1,1) ; (-200,200) ; (600,1000)"
SinOsc freq: f2 phase: 0 mul: 0.1] value

"SinOsc ; Modulate phase"
[|ph|
ph := SinOsc freq: (XLine start: 20 end: 8000 dur: 10 doneAction: 1) phase: 0 mul: 2 * Float pi .
SinOsc freq: 800 phase: ph mul: 0.1] value

"SinOsc ; mouse control"
[|x y|
x := MouseX minval: 40 maxval: 10000 warp: 1 lag: 0.2 .
y := MouseY minval: 0.01 maxval: 0.25 warp: 1 lag: 0.2 .
SinOsc freq: x phase: 0 mul: y] value

"SinOsc ; simple bell-like tone"
[|f a o d e|
f := {0.5 . 1.0 . 1.19 . 1.56 . 2.0 . 2.51 . 2.66 . 3.01 . 4.1} mce .
a := {0.25 . 1.0 . 0.8 . 0.5 . 0.9 . 0.4 . 0.3 . 0.6 . 0.1} mce .
o := SinOsc freq: (f * 500) phase: 0 mul: a .
d := {0.0 . 2.0 . -99.0 . -99.0 . 1.0 . 0.01 . 5.0 . -4.0 . 0.0 . 10.0 . 5.0 . -4.0} mce .
e := EnvGen gate: 1 levelScale: 0.1 levelBias: 0 timeScale: 1 doneAction: 0 envelope: d .
o mix * e] value

"SinOsc ; When two pure tones of slightly different frequency are superposed, our ears
perceive audible beats at a rate given by the difference of the two frequencies."
[|f0 f1 d i|
f0 := 220 .
f1 := 221.25 .
d := (f1 - f0) abs .
i := Impulse freq: d phase: 0 mul: ((SinOsc freq: 0.05 phase: 0 :mul 0.1) max: 0) .
SinOsc freq: {f0 . f1} mce phase: 0 mul: 0.1 add: i] value

---- ; hs

-- sinOsc ; "When two tones are sounded together, a tone of lower frequency is
-- frequently heard. Such a tone is called a combination tone.  The most
-- commonly heard combination tone occurs at a frequency f2 - f1."
let f1 = 300
    f2 = 300 * 3/2 {- 450 -}
    f3 = abs (f2 - f1) {- 150 -}
    a3 = max (sinOsc KR 0.05 0 * 0.1) 0
in mix (sinOsc AR (mce3 f1 f2 f3) 0 * mce3 0.1 0.1 a3)

-- sinOsc ; with frequency of zero, operates as table lookup variant of sin
let mk_phasor (l,r) f = phasor AR 0 ((r - l) * f / sampleRate) l r l
    ph = mk_phasor (0,two_pi) 440
    o1 = sinOsc AR 440 0
    o2 = sinOsc AR 0 ph
    o3 = sin ph
in mce2 o1 (xFade2 o2 o3 (lfTri KR 0.1 0) 1) * 0.1

-- sinOsc ; sync ; i.e. <https://www.listarc.bham.ac.uk/lists/sc-dev/msg58316.html>
let dt = mce2 1.0 1.003
    freq = mouseX KR (100 * dt) (3000 * dt) Exponential 0.2
    sync_freq = mouseY KR 100 500 Exponential 0.2
    ph_freq = impulse AR sync_freq 0 + impulse AR freq 0
    o = sinOsc AR 0 (phasor AR ph_freq (freq / sampleRate) 0 1 0 * 2 * pi)
in o * 0.15

-- sinOsc ; reverse sync
let dt = mce2 1.0 1.003
    freq = mouseX KR (100 * dt) (3000 * dt) Exponential 0.2
    sync_freq = mouseY KR 100 500 Exponential 0.2
    direction = toggleFF (impulse AR sync_freq 0) * (-2) + 1
    o = sinOsc AR 0 (wrap (sweep (impulse AR freq 0) (direction * freq)) 0 (2 * pi))
in o * 0.15

-- sinOsc ; reverse cycle & reverse sync
let freq = mouseX KR (100 * mce2 1.0 1.003) (3000 * mce2 1.0 1.003) Exponential 0.2
    sync_freq = mouseY KR 100 500 Exponential 0.2
    direction = toggleFF (impulse AR sync_freq 0 + impulse AR freq 0) * (-2) + 1
    o = sinOsc AR 0 (wrap (sweep (k2a 0) (direction * freq)) 0 (2 * pi))
in o * 0.5

-- sinOsc ; 15.5 khz
pan2 (sinOsc AR 15500 0) 0 0.75

-- sinOsc ; 12 khz - 15.5 khz sweep
pan2 (sinOsc AR (range_hs (12000,15500) (sinOsc KR (1/6) 0)) 0) 0 0.75

---- ; drawings
Sound.SC3.Plot.plot_ugen_nrt (48000,64) 1.0 (sinOsc AR 1 0)
