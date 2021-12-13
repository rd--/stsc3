"jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157"
(SinOsc freq: (4 ** (200 iota * 0.002) * 300) phase: 0) mix * 0.02

"jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit) ; psuedo-doppler"
(SinOsc freq: (2 ** (100 iota * 0.002) * 120) phase: 0 mul: 0.1) splay

"jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit) ; psuedo-doppler-formants"
(Saw freq: (2 ** (100 iota * 0.002) * 120) mul: 0.1) splay

"jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit)"
|k b n f0 f|
k := 100. "200 ; c.f. 12 25 50 100 200 400 600 800"
b := 2. "4 ; c.f. 2, 3, 4"
n := 0.02. "0.002 ; c.f. 0.002 0.004 0.01 0.05 0.1 0.125"
f0 := 180. "300 ; c.f. 120 180 240"
f := b ** (k iota * n).
(SinOsc freq: f * f0 phase: 0 mul: 0.1) splay

"jh ; https://scsynth.org/t/auditory-illusion-with-exponentially-spaced-frequencies/4157 ; rd (edit)"
| t sinosc k b n f0 fMul e |
t := Impulse freq: 0.1 phase: 0.
sinosc := [ :freq | (Phasor trig: t rate: SampleDur new * freq start: 0 end: 1 resetPos: 0 mul: 2 * Float pi) sin ].
k := 160.
b := TRand lo: 2 hi: 2.25 trig: t.
n := TRand lo: 0.002 hi: 0.02 trig: t.
f0 := TRand lo: 90 hi: 180 trig: t.
fMul := b ** (k iota * n).
e := Decay2 in: t attackTime: 1 decayTime: 10 mul: 0.1.
Splay input: (sinosc value: fMul * f0) level: e
