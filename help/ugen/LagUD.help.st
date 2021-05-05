"LagUD ; event control"
| f |
f := [:e :i |
 | o w |
 o := SinOsc freq: (e x * 24 + 48) midicps phase: 0.
 w := LagUD in: e w lagTimeU: 0.001 lagTimeD: 2.0.
 Pan2 in: o pos: e o * 2 - 1 level: e z * w kr * 0.5].
(EventControl numVoices: 16 voiceFunc: f) mix
