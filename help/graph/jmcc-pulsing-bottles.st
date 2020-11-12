"pulsing bottles (jmcc) #2"
n := 6 . "number of bottles"
f := [:_x|
      |a l|
      a := (LFPulse freq: (4 + (10.0 rand)) iphase: 0 width: (0.7 rand)) * (0.8 / n) .
      l := SinOsc freq: (0.1 + (0.4 rand)) phase: (2 * Float pi) rand .
      Pan2 in: (Resonz in: (WhiteNoise new * (a kr)) freq: (400 + 7000.0 rand) bwr: 0.01) pos: (l kr) level: 1] . "linrand"
o := n mixFill: f .
o
