"http://sccode.org/1-4Qx (jar)"
| f g h s |
f := LFCub freq: 9.1 iphase: 0 mul: 100 add: #(200 300 400 500).
g := LFCub freq: {1/2. 1/3. 1/5. 1/7} iphase: 0 mul: 0.5 add: (LFCub freq: 9 iphase: 0).
h := LFCub freq: 1/2 iphase: 0 mul: 0.4 add: 0.5.
s := LFCub freq: f kr iphase: 0 mul: (Lag in: g > h lagTime: 0.1) kr.
Splay input: s spread: 1 level: 0.075 center: 0 levelComp: true
