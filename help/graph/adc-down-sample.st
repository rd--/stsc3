|f nh src sr|
f := LFNoise2 kr: {8} .
nh := LFNoise2 kr: {3} .
src := Blip ar: {f * 200 + 300 . nh * 10 + 20} .
sr := MouseX kr: {1000 . SampleRate ir * 0.1 . 1 . 0.2} .
Latch ar: {src . Impulse ar: {sr . 0}}
