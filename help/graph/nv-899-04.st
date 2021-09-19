"nv https://swiki.hfbk-hamburg.de/MusicTechnology/899"
| s f a |
s := LocalIn numChan: 2 default: 0 mul: 7.5 add: (Saw freq: #(32 33) mul: 0.2).
f := (2 ** (LFNoise0 freq: 4/3 mul: 4)) * 300.
a := CombN in: (BPF in: s freq: f rq: 0.1) distort maxdelaytime: 2 delaytime: 2 decaytime: 40.
Mrg lhs: a rhs: (LocalOut channelsArray: a)
