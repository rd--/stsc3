"pulsing bottles (jmcc) #2"
| n f |
n := 6. "number of bottles"
f := [
	| a l |
	a := LfPulse freq: (4 rand: 14) iphase: 0 width: (0.7 rand) mul: (0.8 / n).
	l := SinOsc freq: (0.1 rand: 0.5) phase: (2 * Float pi) rand.
	Pan2 in: (Resonz in: (WhiteNoise mul: a kr) freq: (400 rand: 7400) bwr: 0.01) pos: (l kr) level: 1]. "linrand"
(f dup: n) sum
