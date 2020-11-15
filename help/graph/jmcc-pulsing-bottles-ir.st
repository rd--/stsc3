"pulsing bottles (jmcc) #2"
|n f o|
n := 6 . "number of bottles"
f := [:_x|
      |a l|
      a := LFPulse freq: (Rand lo: 4 hi: 14) iphase: 0 width: (Rand lo: 0 hi: 0.7) mul: (0.8 / n) .
      l := SinOsc freq: (Rand lo: 0.1 hi: 0.5) phase: (Rand lo: 0 hi: (2 * Float pi)) .
      Pan2 in: (Resonz in: (WhiteNoise mul: a kr) freq: (Rand lo: 400 hi: 7400) bwr: 0.01) pos: (l kr) level: 1] . "linrand"
o := n mixFill: f .
o
