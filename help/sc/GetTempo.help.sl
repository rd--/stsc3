# GetTempo -- continuous tempo

_GetTempo()_

Gives the current tempo as a control rate signal. Tempo is always in beats per second.

	SetTempo(MouseX(0.25, 4, 1, 0.2))

Use the tempo to modulate an oscillator:

	SinOsc(400 * GetTempo(), 0, 0.2);

