"Pan2 ; note level input is control rate only"
Pan2 in: (SinOsc freq: 440 phase: 0) pos: (SinOsc freq: 0.5 phase: 0) kr level: 0.1

"Pan2"
Pan2 in: WhiteNoise new pos: (MouseX minval: -1 maxval: 1 warp: 0 lag: 0.2) kr level: 0.1
