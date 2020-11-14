"sprinkler (jmcc) #1"
|f t o|
f := MulAdd in: (LFPulse freq: 0.09 iphase: 0 width: 0.16) mul: 10 add: 7 .
t := (LFPulse freq: f iphase: 0 width: 0.25) * 0.1 .
o := BPZ2 in: (WhiteNoise new * (t kr)) .
o
