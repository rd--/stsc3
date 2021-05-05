"blip ; event control"
| f |
f := [:e :i |
  | o |
  o := (Blip freq: (e x * 13 + 48) midicps numharm: e y * 19 + 1).
  Pan2 in: o pos: e o * 2 - 1 level: e z * e w * 0.5].
(EventControl numVoices: 16 voiceFunc: f) mix
