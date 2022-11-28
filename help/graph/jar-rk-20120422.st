"rukano; Apr 22, 2012; Re: Epic Pads (jar)"
| freq gen fmod rqmod sig amp |
freq := [ (LfNoise2 freq: 1 mul: 0.011 add: 1) * #(60 64 65 67) atRandom midiCps ] dup: 24.
gen := LfSaw freq: freq iphase: 0 mul: 0.1.
fmod := LinLin in: (LfCub freq: (1 / 12) iphase: 0) srclo: -1 srchi: 1 dstlo: 1 dsthi: (MouseX minval: 2 maxval: 16 warp: 0 lag: 0.2).
rqmod := LfNoise2 freq: 1 mul: 0.011 add: 1.
sig := Rlpf in: gen freq: (freq * fmod) kr rq: rqmod kr.
amp := MouseY minval: 0 maxval: 0.25 warp: 0 lag: 0.2.
sig splay2 * amp kr
