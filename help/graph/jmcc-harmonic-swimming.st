"harmonic swimming (jmcc) #1"
|a f p l o s|
a := 0.02 . "amplitude scale"
f := 50 . "fundamental frequency"
p := 20 . "number of partials per channel"
l := Line start: 0 end: (a negated) dur: 60 doneAction: 0 . "amplitude offset, causes sound to separate and fade"
o := [:h | "harmonic number (from one)"
      | r n e |
      r := (2 mceFill: [:_x | Rand lo: -4 hi: 4]) + 6 . "amplitude rate"
      n := LFNoise1 freq: r mul: a add: l .
      e := (n max: 0) kr . "clip negative amplitudes to zero"
      FSinOsc freq: (f * h) iphase: 0 mul: e] .
s := p mixFill: o .
s
