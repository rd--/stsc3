"membraneCircle ; event control ; note limited voice count"
| f |
f := [ :e |
	| loss wobble tension env exc |
	loss := LinExp in: e y srclo: 0 srchi: 1 dstlo: 0.99999 dsthi: 0.99950.
	wobble := SinOsc freq: 2 phase: 0.
	tension := (LinExp in: e x srclo: 0 srchi: 1 dstlo: 0.01 dsthi: 0.1) + (wobble * 0.0001).
	env := Perc trig: e w attackTime: 0.0001 releaseTime: (1 - e z) * 0.1 curve: 1.
	exc := PinkNoise mul: e y / 4 + e z * env.
	Pan2 in: (MembraneCircle excitation: exc tension: tension loss: loss) pos: e o * 2 - 1 level: 0.1 ].
(Voicer numVoices: 6 voiceFunc: f) sum
