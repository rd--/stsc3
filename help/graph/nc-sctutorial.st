"https://composerprogrammer.com/teaching/supercollider/sctutorial/tutorial.html 1.1"
| n mk x y |
n := 11.
mk := [:i |
 | freq numcps knum gen |
 freq := 50.0 rand: 560.3.
 numcps := 2 rand: 20.
 knum := SinOsc freq: (0.02 exprand: 0.2) phase: 0 mul: numcps / 2 add: numcps / 2.
 gen := Gendy1 ampdist: 6 rand durdist: 6 rand adparam: 1.0 rand ddparam: 1.0 rand minfreq: freq maxfreq: freq ampscale: 1.0 rand durscale: 1.0 rand initCPs: numcps knum: knum kr mul: 0.5 / n sqrt.
 Pan2 in: gen pos: 1.0 rand2 level: 1.0].
x := MouseX minval: 100 maxval: 2000 warp: 0 lag: 0.2.
y := MouseY minval: 0.01 maxval: 1.0 warp: 0 lag: 0.2.
Resonz in: (n mixFill: mk) freq: x kr bwr: y kr mul: 0.5
