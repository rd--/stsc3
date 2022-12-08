"http://sccode.org/1-4Qx (jar)"
| f g h s |
f := LfCub freq: 9.1 iphase: 0 mul: 100 add: #(200 300 400 500).
g := LfCub freq: {1/2. 1/3. 1/5. 1/7} iphase: 0 mul: 0.5 add: (LfCub freq: 9 iphase: 0).
h := LfCub freq: 1/2 iphase: 0 mul: 0.4 add: 0.5.
s := LfCub freq: f kr iphase: 0 mul: (Lag in: g > h lagTime: 0.1) kr.
s splay2 * 0.075
