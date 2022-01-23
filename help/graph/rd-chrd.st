"chrd (rd) ; graph rewrite"
| gen |
gen := [ :tr |
  | chrd scale octaves mnn chd |
  chrd := [ :m |
    | ds d f e p o |
    ds := 3.
    d := #(5 4 5 7 4 5) * ds.
    f := (TXLine start: m end: (TRand lo: 0.05 hi: 0.5 trig: tr) + m dur: d trig: tr) midiCps.
    e := (EnvSine dur: d max level: (TRand lo: 0.005 hi: 0.01 trig: tr)) asEnvGen: tr.
    p := TXLine start: (TRand lo: -1 hi: 1 trig: tr) end: (TRand lo: -1 hi: 1 trig: tr) dur: d trig: tr.
    o := SinOsc freq: f kr phase: 0.
    (Pan2 in: o pos: p level: e) mix ].
  scale := #(0 2 4 5 7 9 11).
  octaves := #(4 5 6 7).
  mnn := (scale collect: [ :n | octaves collect: [ :o | n + (o * 12) ] ]) concatenation.
  chd := [ TChoose trig: tr array: mnn ] dup: 6.
  9 mixFill: [ chrd value: chd ] ].
OverlapTexture graphFunc: gen sustainTime: 21 transitionTime: 0 overlap: 3
