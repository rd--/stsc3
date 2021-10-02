"membraneCircle ; event control ; note limited voice count"
| f |
f := [ :e :ix |
  | loss wobble tension env exc |
  loss := LinExp in: e y srclo: 0 srchi: 1 dstlo: 0.99999 dsthi: 0.99950.
  wobble := SinOsc freq: 2 phase: 0.
  tension := (LinExp in: e x srclo: 0 srchi: 1 dstlo: 0.01 dsthi: 0.1) + (wobble * 0.0001).
  env := (EnvPerc attackTime: 0.0001 releaseTime: (1 - e z) * 0.1 level: 1 curves: #(1)) envGen: e w.
  exc := PinkNoise mul: e y / 4 + e z * env kr.
  Pan2 in: (MembraneCircle excitation: exc tension: tension kr loss: loss kr) pos: e o * 2 - 1 level: 0.1 ].
(EventControl numVoices: 6 voiceFunc: f) mix
