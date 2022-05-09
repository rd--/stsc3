"sample and hold liquidities (jmcc) #4"
| r t c cf f p s i |
r := MouseX minval: 1 maxval: 200 warp: 1 lag: 0.2.
t := r reciprocal.
c := Impulse freq: r phase: 0 mul: 0.4.
cf := MouseY minval: 100 maxval: 8000 warp: 1 lag: 0.1.
f := Latch in: (WhiteNoise new * cf * 0.5 + cf) trig: c.
p := Latch in: (WhiteNoise new) trig: c.
s := SinOsc freq: f phase: 0 mul: (Decay2 in: c attackTime: t * 0.1 decayTime: t * 0.9).
i := Pan2 in: s pos: p level: 1.
CombN in: i maxdelaytime: 0.3 delaytime: 0.3 decaytime: 2
