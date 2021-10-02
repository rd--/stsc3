"combc ; event control"
| lfo osc |
lfo := SinOsc freq: 0.5 phase: 0.
osc := EventControl numVoices: 16 voiceFunc: [ :e | SinOsc freq: e p midicps phase: 0 mul: lfo * e w * e z ].
CombC in: osc splay maxdelaytime: 0.5 delaytime: 0.2 decaytime: 3
