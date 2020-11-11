"amplitude"
a := Amplitude in: (AR :source (SoundIn bus: 0)) attackTime: 0.01 releaseTime: 0.01
f := MulAdd in: a mul: 1200 add: 400
SinOsc freq: (KR source: f) phase: 0 * 0.1
