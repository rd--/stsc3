"combc ; event control"
| lfo osc |
lfo := SinOsc freq: 0.5 phase: 0.
osc := Voicer numVoices: 16 voiceFunc: [ :e | SinOsc freq: e p unitCps phase: 0 mul: lfo * e w * e z ].
CombC in: osc splay2 maxdelaytime: 0.5 delaytime: 0.2 decaytime: 3
