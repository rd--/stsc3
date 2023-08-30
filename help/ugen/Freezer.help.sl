(* Freezer ; static instance *)
var buf = 0;
Freezer(buf, 0.35, [0.4, 0.45], 0.6, 0.1, 0.05, [0.02, 0.05], 0.1, 0, 0, [6, 8])

(* Freezer ; static instance *)
var buf = 0;
Freezer(buf, 0.3, [0.4, 0.45], 0.6, [0.98, 1], [0.001, 0.005], 0, 0, 0, 0, 6)

(* Freezer ; static instance *)
var buf = 0;
Freezer(buf, 0.3, [0.7, 0.75], 0.6, 0.35, 0, 0.5, 0.5, 0, 0, 6)

(* Freezer ; static instance *)
var buf = 0;
Freezer(buf, 0.2500, 0.2505, [0.1, 0.2], 1, 0, 0.050, [0.005, 0.010], 0, 0, 16)

(* Freezer ; k-rate instance *)
var buf = 0;
var n = { :f :i :j | LinLin(LfNoise2(f), -1, 1, i, j) };
var left = n(1, 0.3, [0.6, 0.8]);
var right = left + n(1, 0.01, [0.05, 0.1]);
Freezer(buf, left, right, [0.1, 0.2], 0.5, 0.1, 0.5, 0.05, 0, 0, 24)

(* Freezer ; k-rate instance *)
var buf = 0;
var n = { :i :j | LinLin(LfNoise2(0.1), -1, 1, i, j) };
Freezer(buf, n(0.3, [0.4, 0.5]), n(0.5, [0.6, 0.7]), n(0.3, [0.6, 0.8]), n(0.95, 1.05), n(0.05, 0.15), n(0.05, 0.15), n(0.05, 0.15), 0, 0, 36)
