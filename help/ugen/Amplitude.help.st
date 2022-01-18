"---- ; Amplitude ; Amplitude operates at .kr, In operates at .ar"
| i a |
i := In numChan: 1 bus: (Control name: 'bus' init: 0).
a := Amplitude in: i ar attackTime: 0.01 releaseTime: 0.01 mul: 1200 add: 400.
SinOsc freq: a kr phase: 0 mul: 0.1
