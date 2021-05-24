"random sine waves (jmcc) #1 ; texture=spawn,0.75,inf"
| env osc |
env := EnvLinen attackTime: 2 sustainTime: 5 releaseTime: 2 level: 0.02 curves: #(#lin #lin #lin).
osc := FSinOsc freq: (2000.0 rand) iphase: 0 mul: (env envGen: 1 doneAction: 2) kr.
Pan2 in: osc pos: 1.0 rand2 level: 1
