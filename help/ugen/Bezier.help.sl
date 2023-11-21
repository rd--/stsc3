(* Bezier ; reference sine tone *)
SinOsc(440, 0) * 0.1

(* Bezier ; Bézier curve oscillator ; bezier approximation of sin function *)
Bezier(100, 0.001, 440, 0, [0, 0, 0.2, -1.3, 0.3, -1.3, 0.5, 0, 0.7, 1.3, 0.8, 1.3, 1, 0]) * 0.1

(* Bezier ; mouse control of frequency *)
var x = MouseX(40, 4000, 1, 0.2);
Bezier(100, 0.001, x, 0, [0, 0, 0.2, -1.3, 0.3, -1.3, 0.5, 0, 0.7, 1.3, 0.8, 1.3, 1, 0]) * 0.1

(* Bezier ; mouse control of frequency & one control point *)
var x = MouseX(40, 4000, 1, 0.2);
var y = MouseY(0.1, 0.2, 0, 0.2);
Bezier(100, 0.001, x, 0, [0, 0, y, -1.3, 0.3, -1.3, 0.5, 0, 0.7, 1.3, 0.8, 1.3, 1, 0]) * 0.1

(* Bezier ; dynamic shifting of control points, as oscillator *)
var w = { :l :r |
	LinLin(LfdNoise3(2), -1, 1, l, r)
};
{
	var p = [
		0, 0,
		w(0.05, 0.25), -1.3,
		w(0.25, 0.45), -1.3,
		0.5, 0,
		w(0.55, 0.75), 1.3,
		w(0.75, 0.95), 1.3,
		1, 0
	];
	var o = Bezier(100, 0.001, w(200, 205), 0, p);
	EqPan2(o, IRand(-1, 1)) * 0.1
} !> 4

(* Bezier ; event control ; requires=voicer *)
Voicer(16) { :e |
	var w = { :l :r |
		LinLin(LfdNoise3(e.j * 5), -1, 1, l, r)
	};
	{
		var p = [
			0, 0,
			w(0.05, 0.25), -1.3,
			w(0.25, 0.45), -1.3,
			0.5, 0,
			w(0.55, 0.75), 1.3,
			w(0.75, 0.95), 1.3,
			1, 0
		];
		var o = Bezier(100, 0.001, w(200, 205) * (e.x + 0.5), 0, p);
		EqPan2(o, e.i * 2 - 1) * e.z * LagUd(e.w, 0.01, e.y * 4)
	} !+ 4
}.Mix (* Cpu Load: Mac Studio M1 Max 2022: 16 voices = 85% *)
