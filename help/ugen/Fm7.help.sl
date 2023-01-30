;; Fm7 ; two of six
var ctlMatrix = [
	[XLn(300, 310, 4), 0, 1],
	[XLn(300, 310, 8), 0, 1],
	[0, 0, 1],
	[0, 0, 1],
	[0, 0, 1],
	[0, 0, 1]
];
var modMatrix = [
	[Ln(0, 0.001, 2), Ln(0.1, 0, 4), 0, 0, 0, 0],
	[Ln(0, 6, 1), 0, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0],
	[0, 0, 0, 0, 0, 0]
];
Fm7(ctlMatrix, modMatrix).first(2) * 0.1

;; Fm7
var freq = LfNoise0(3).ExpRange(200, 310);
var ctlMatrix = [
	[freq, 0, 1],
	[freq, 0, 1],
	[LfNoise2(0.5).ExpRange(3, 80), 0, 1],
	[LfNoise2(0.5).ExpRange(3, 800), 0, 1],
	[LfNoise2(0.5).ExpRange(3, 800), 0, 1],
	[LfNoise2(0.5).ExpRange(0.3, 10), 0, 1]
];
var x = MouseX(0, 3, 0, 0.2);
var modMatrix = { { LfNoise1(0.5).Max(0) } ! 6 * x } ! 6;
Fm7(ctlMatrix, modMatrix).first(2) * -12.DbAmp

;; Fm7 ; an algorithmically generated graph courtesy f0 ; note one-indexing
var xMatrix = [
	[
		[0.0, -1/3, -1.0, 0.0],
		[0.75, 0.75, 0.0, -0.5],
		[-0.5, -0.25, 0.25, -0.75],
		[-0.5, 1.0, 1.0, 1.0],
		[0.0, 1/6, -0.75, -1.0],
		[0.5, 0.5, -0.5, 1/3]
	],
	[
		[-1/3, 0.5, -0.5, -0.5],
		[0.5, 0.75, 0.25, 0.75],
		[-15/18, 0.25, -1.0, 0.5],
		[1.5, 0.25, 0.25, -0.25],
		[-2/3, -2/3, -1.0, -0.5],
		[-1.0, 0.0, -15/18, -1/3]
	],
	[
		[0.25, -0.5, -0.5, -1.0],
		[-0.5, 1.0, -1.5, 0.0],
		[-1.0, -1.5, -0.5, 0.0],
		[0.5, -1.0, 7/6, -0.5],
		[15/18, -0.75, -1.5, 0.5],
		[0.25, -1.0, 0.5, 1.0]
	],
	[
		[1.0, 1/3, 0.0, -0.75],
		[-0.25, 0.0, 0.0, -0.5],
		[-0.5, -0.5, 0.0, 0.5],
		[1.0, 0.75, 0.5, 0.5],
		[0.0, 1.5, -0.5, 0.0],
		[1.0, 0.0, -0.25, -0.5]
	],
	[
		[0.5, -0.25, 0.0, 1/3],
		[0.25, -0.75, 1/3, -1.0],
		[-0.25, -0.5, 0.25, -7/6],
		[0.0, 0.25, 0.5, 1/6],
		[-1.0, -0.5, 15/18, -0.5],
		[15/18, -0.75, -0.5, 0.0]
	],
	[
		[0.0, -0.75, -1/6, 0.0],
		[1.0, 0.5, 0.5, 0.0],
		[-0.5, 0.0, -0.5, 0.0],
		[-0.5, -1/6, 0.0, 0.5],
		[-0.25, 1/6, -0.75, 0.25],
		[-7/6, -4/3, -1/6, 1.5]
	]
];
var yMatrix = [
	[
		[0.0, -0.5, 1.0, 0.0],
		[-0.5, 1.0, 0.5, -0.5],
		[0.0, 1/3, 1.0, 1.0]
	],
	[
		[-0.5, 0.5, 1.0, 1.0],
		[0.0, 1/3, 0.0, 1.5],
		[-0.5, 15/18, 1.0, 0.0]
	],
	[
		[0.25, -2/3, 0.25, 0.0],
		[0.5, -0.5, -0.5, -0.5],
		[0.5, -0.5, -0.75, 15/18]
	],
	[
		[-0.25, 1.0, 0.0, 1/3],
		[-1.25, -0.25, 0.5, 0.0],
		[0.0, -1.25, -0.25, -0.5]
	],
	[
		[0.75, -0.25, 1.5, 0.0],
		[0.25, -1.5, 0.5, 0.5],
		[-0.5, -0.5, -0.5, -0.25]
	],
	[
		[0.0, 0.5, -0.5, 0.25],
		[0.25, 0.5, -1/3, 0.0],
		[1.0, 0.5, -1/6, 0.5]
	]
];
var ctlMatrix = xMatrix.collect { :p | p.collect { :q | SinOsc(q[1], q[2]) * q[3] + q[4] } };
var modMatrix = yMatrix.collect { :p | p.collect { :q | Pulse(q[1], q[2]) * q[3] + q[4] } };
var o = Fm7(ctlMatrix, modMatrix);
var g3 = LinLin(LfSaw(0.1, 0), -1, 1, 0, -12.DbAmp);
var g6 = -3.DbAmp;
[o[1] + (o[3] * g3) + o[5], o[2] + o[4] + o[6] * g6]
