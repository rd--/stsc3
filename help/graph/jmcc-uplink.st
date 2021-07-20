"uplink ; jmcc"
|osc freq|
osc := [
 |e|
 e := (LFPulse kr: {Rand ir: {0 . 4} . 0 . Rand ir: {0 . 1}}) * (Rand ir: {0 . 8000}) + (Rand ir: {0 . 2000}) .
 (LFPulse kr: {Rand ir: {0 . 20} . 0 . Rand ir: {0 . 1}}) * e] .
freq := osc value + osc value .
Pan2 ar: {(LFPulse ar: {freq . 0 . 0.5}) * 0.04 . Rand ir: {0 . 0.8}}

"uplink ; jmcc"
|osc freq sig|
 osc := [
 |e|
 e := (LFPulse kr: {#freq: -> (Rand ir: {#lo: -> 0. #hi: -> 4}). #iphase: -> 0. #width: -> (Rand ir: {#lo: -> 0. #hi: -> 1})}) * (Rand ir: {#lo: -> 0. #hi: -> 8000}) + (Rand ir: {#lo: -> 0. #hi: -> 2000}).
 (LFPulse kr: {#freq: -> (Rand ir: {#lo: -> 0. #hi: -> 20}). #iphase: -> 0. #width: -> (Rand ir: {#lo: -> 0. #hi: -> 1})}) * e].
freq := osc value + osc value.
sig := (LFPulse ar: {#freq: -> freq. #iphase: -> 0. #width: -> 0.5}) * 0.04.
Pan2 ar: {#in: -> sig. #pos: -> (Rand ir: {#lo: -> 0. #hi: -> 0.8})}.
