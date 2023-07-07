# Spring -- physical model of resonating spring

_Spring(in, spring, damp)_

Models the force of a resonating string

- in: modulated input force
- spring: spring constant (incl. mass)
- damp: damping

Trigger gate is mouse button, spring constant is _MouseX_, _MouseY_ controls damping, modulate frequency with the force:

	var inforce = K2A(MouseButton(0, 1, 0)) > 0;
	var k = MouseY(0.1, 20, 1, 0.2);
	var d = MouseX(0.00001, 0.1, 1, 0.2);
	var outforce = Spring(inforce, k, d);
	var freq = outforce * 400 + 500;
	SinOsc(freq, 0) * 0.2

Several springs in series. Trigger gate is mouse button, spring constant is _MouseX_, _MouseY_ controls damping, modulate frequency with the force:

	var d = MouseY(0.00001, 0.01, 1, 0.2);
	var k = MouseX(0.1, 20, 1, 0.2);
	var inforce = K2A(MouseButton(0, 1, 0)) > 0;
	var m0 = Spring(inforce, k, 0.01);
	var m1 = Spring(m0, 0.5 * k, d);
	var m2 = Spring(m0, 0.6 * k + 0.2, d);
	var m3 = Spring(m1 - m2, 0.4, d);
	SinOsc(m3 * 200 + 500, 0) * 0.1

Modulating a resonating string with the force, spring constant is _MouseX_, _MouseY_ controls damping:

	var k = MouseX(0.5, 100, 1, 0.2);
	var d = MouseY(0.0001, 0.01, 1, 0.2);
	var t = Dust(2);
	var m0 = Spring(ToggleFf(t), 1 * k, 0.01);
	var m1 = Spring(m0, 0.5 * k, d);
	var m2 = Spring(m0, 0.6 * k, d);
	var m3 = Spring([m1, m2], 0.4 * k, d);
	var m4 = Spring(m3 - m1 + m2, 0.1 * k, d);
	CombL(t, 0.1, LinLin(m4, -10, 10, 1/8000, 1/100), 12)
