"rukano; Apr 22, 2012; Re: Epic Pads (jar)"
| freq gen fmod rqmod sig amp |
freq := 24 mceFill: [:i | (LFNoise2 freq: 1 mul: 0.011 add: 1) * #(60 64 65 67) atRandom midicps].
gen := LFSaw freq: freq iphase: 0 mul: 0.1.
fmod := (LFCub freq: (1 / 12) iphase: 0) inRangeFrom: 1 constant to: (MouseX minval: 2 maxval: 16 warp: 0 lag: 0.2).
rqmod := LFNoise2 freq: 1 mul: 0.011 add: 1.
sig := RLPF in: gen freq: (freq * fmod) kr rq: rqmod kr.
amp := MouseY minval: 0 maxval: 0.25 warp: 0 lag: 0.2.
(Splay input: sig) * amp kr
