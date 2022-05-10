"moogLadder"
| o cf |
o := (LFSaw freq: #(120 180) iphase: 0 mul: 0.33) mix.
cf := LinExp in: (LFCub freq: 0.1 iphase: (0.5 * Float pi)) srclo: -1 srchi: 1 dstlo: 180 dsthi: 8500.
MoogLadder in: o ffreq: cf kr res: 0.75

"moogLadder ; wind"
| mk |
mk := [ :k |
    | n freq force |
    n := WhiteNoise new.
    force := LinLin in: (LFNoise2 freq: 0.25) kr srclo: -1 srchi: 1 dstlo: 0.25 dsthi: 0.75.
    freq := (force * 88 + 20) midiCps.
    MoogLadder in: n ffreq: freq res: force
].
(1 to: 6 collect: mk) splay2
