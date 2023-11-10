"TChoose ; select input at trigger"
| x t f o |
x := MouseX minval: 1 maxval: 1000 warp: 1 lag: 0.1.
t := Dust density: x.
f := (TiRand lo: 48 hi: 60 trig: t) midiCps.
o := TChoose trig: t array: {SinOsc freq: f phase: 0. Saw freq: (f * 2). Pulse freq: f * 0.5 width: 0.1}.
o * 0.1
