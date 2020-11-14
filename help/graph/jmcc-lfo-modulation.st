"lfo modulation (jmcc) #1"
|o p s c|
o := (FSinOsc freq: 0.05 iphase: 0) * 80 + 160 .
p := (FSinOsc freq: {0.6 . 0.7} iphase: 0) * 3600 + 4000 .
s := RLPF in: ((LFPulse freq: (o kr) iphase: 0 width: 0.4) * 0.05) freq: (p kr) rq: 0.2 .
c := CombL in: s maxdelaytime: 0.3 delaytime: {0.2 . 0.25} decaytime: 2 .
c

