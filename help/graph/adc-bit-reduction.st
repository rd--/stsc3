"bit-reduction (adc) ; autotranslate"
|f nh src sr bitSz downSample bitRedux|
f := LFNoise2 kr: {8}.
nh := LFNoise2 kr: {3}.
src := Blip ar: {f * 200 + 300. nh * 10 + 20}.
sr := MouseX kr: {1000. SampleRate ir * 0.1. 1. 0.2}.
bitSz := MouseY kr: {1. 24. 1. 0.2}.
downSample := Latch ar: {src. Impulse ar: {sr. 0}}.
bitRedux := downSample round: {0.5 constant ** bitSz}.
{downSample. bitRedux} mce
