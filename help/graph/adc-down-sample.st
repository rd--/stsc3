"down-sample (adc) ; autotranslate"
|f nh src sr|
f := LFNoise2 kr: #(8 200 300).
nh := LFNoise2 kr: #(3 10 20).
src := Blip ar: {f. nh}.
sr := MouseX kr: {1000. SampleRate ir: {0.1}. 1. 0.2}.
Latch ar: {src. Impulse ar: {sr. 0}}
