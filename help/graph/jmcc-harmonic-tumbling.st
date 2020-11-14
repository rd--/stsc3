"harmonic tumbling (jmcc) #1"
|f p t o s|
f := 80 .
p := 10 .
t := XLine start: {10 . 11} end: 0.1 dur: 60 doneAction: 0 . "trigger probability decreases over time"
o := [:h |
     | n r e |
     n := Dust density: t .
     r := Rand lo: 0 hi: 0.5 .
     e := Decay2 in: (n * 0.02) attackTime: 0.005 decayTime: r .
     (FSinOsc freq: (f * h) iphase: 0) * (e kr)] .
s := p mixFill: o .
s
