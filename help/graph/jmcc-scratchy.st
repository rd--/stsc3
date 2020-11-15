"scratchy (jmcc) #1"
|n f o|
n := (2 mceFill: [:_x| BrownNoise new]) * 0.5 .
f := ((n - 0.49) max: 0) * 20 .
o := RHPF in: f freq: 5000 rq: 1 .
o
