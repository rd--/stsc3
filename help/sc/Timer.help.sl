# Timer -- returns time since last triggered

_Timer(trig)_

Using timer to modulate sine frequency, the slower the trigger is the higher the frequency:

	var x = MouseX(0.5, 20, 1, 0.2);
	var trig = Impulse(x, 0);
	SinOsc([x * 20 + 100, Timer(trig) * 500 + 500], 0) * 0.1

