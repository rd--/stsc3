"bit-reduction (adc)"
|f nh src sr bit_sz down_sample bit_redux|
f := LFNoise2 kr: {8} .
nh := LFNoise2 kr: {3} .
src := Blip ar: {f * 200 + 300 . nh * 10 + 20} .
sr := MouseX kr: {1000 . SampleRate ir * 0.1 . 1 . 0.2} .
bit_sz := MouseY kr: {1 . 24 . 1 . 0.2} .
down_sample := Latch ar: {src . Impulse ar: {sr . 0}} .
bit_redux := down_sample round: {0.5 constant ** bit_sz} .
{down_sample . bit_redux} mce
