"uplink ; jmcc"
| osc freq sig |
 osc := [
 | e |
 e := (LFPulse freq: (Rand lo: 0 hi: 4) iphase: 0 width: (Rand lo: 0 hi: 1)) mul: (Rand lo: 0 hi: 8000) add: (Rand lo: 0 hi: 2000).
 LFPulse freq: (Rand lo: 0 hi: 20) iphase: 0 width: (Rand lo: 0 hi: 1) mul: e ].
freq := osc value + osc value.
sig := LFPulse freq: freq kr iphase: 0 width: 0.5 mul: 0.04.
Pan2 in: sig pos: (Rand lo: 0 hi: 0.8) level: 1
