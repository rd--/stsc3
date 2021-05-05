"pluck ; event control"
| f |
f := [:e :i |
 | n dm dl sig |
 n := WhiteNoise mul: e z.
 dm := 1 / 220.
 dl := (e x negated * 0.9 + 1) * dm.
 sig := Pluck in: n trig: e w maxdelaytime: dm delaytime: dl decaytime: 10 coef: (e y / 3).
 Pan2 in: sig pos: e o * 2 - 1 level: (Control name: 'gain' init: 2)].
(EventControl numVoices: 16 voiceFunc: f) mix
