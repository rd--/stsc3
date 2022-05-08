"uplink ; jmcc ; graph rewrite"
| uplink |
uplink := [ :tr |
  | osc freq sig |
   osc := [
    | e r |
    r := [ :hi | TRand lo: 0 hi: hi trig: tr ].
    e := (LFPulse freq: (r value: 4) iphase: 0 width: (r value: 1)) mul: (r value: 8000) add: (r value: 2000).
    LFPulse freq: (r value: 20) iphase: 0 width: (r value: 1) mul: e
  ].
  freq := osc value + osc value.
  sig := LFPulse freq: freq kr iphase: 0 width: 0.5 mul: 0.04.
  Pan2 in: sig pos: (TRand lo: -1 hi: 1 trig: tr) level: 1
].
OverlapTexture graphFunc: uplink sustainTime: 4 transitionTime: 1 overlap: 5

"uplink ; jmcc ; texture=overlap,4,1,5,inf"
| osc freq sig |
 osc := [
  | e |
  r := [ :hi | Rand lo: 0 hi: hi ].
  e := (LFPulse freq: (r value: 4) iphase: 0 width: (r value: 1)) mul: (r value: 8000) add: (r value: 2000).
  LFPulse freq: (r value: 20) iphase: 0 width: (r value: 1) mul: e
].
freq := osc value + osc value.
sig := LFPulse freq: freq kr iphase: 0 width: 0.5 mul: 0.04.
Pan2 in: sig pos: (Rand lo: -1 hi: 1) level: 1
