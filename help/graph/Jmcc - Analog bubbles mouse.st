"analog bubbles with mouse control (jmcc) #3"
| y x lfo_2 lfo_1 freq |
y := MouseY minval: 0.1 maxval: 10 warp: 1 lag: 0.2. "lfo 1 rate"
x := MouseX minval: 2 maxval: 40 warp: 1 lag: 0.2.  "lfo 2 rate"
lfo_2 := LfSaw freq: x iphase: 0 mul: -3 add: 80. "depth & offset in semitones"
lfo_1 := LfSaw freq: y iphase: 0 mul: 24 add: lfo_2. "depth in semitones, offset is lfo_2"
freq := lfo_1 midiCps kr. "convert to frequency"
CombN in: (SinOsc freq: freq phase: 0 mul: 0.04) maxdelaytime: 0.2 delaytime: 0.2 decaytime: 2 "echoing sine wave"
