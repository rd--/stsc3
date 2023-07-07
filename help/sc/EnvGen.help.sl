# EnvGen - Envelope Generator

- _EnvGen(gate=1, levelScale=1, levelBias=0, timeScale=1, doneAction=0, envelope)_

Plays back break point envelopes. The envelopes are instances of the Env class. See the Env help file for more info.

If the gate input is not supplied then the envelope begins at time zero of the event in which it is spawned and is released when the event is released. And envelope only has a physical end time (used to cut off events in a Spawn) if the gate input is not supplied.

- gate: The gate input will trigger an attack and hold a sustain of the envelope.
- levelScale: scales the levels of the breakpoints.
- levelBias: offsets the levels of the breakpoints.
- timeScale: scales the breakpoint durations.
- doneAction: what to do at the end of the envelope.
- envelope: an instance of Env.

Slow envelope with slow trigger:

	var trg = Impulse(1 / 9, 0);
	var env = Env([0, 0.1, 0.1, 0], [3, 2, 3], 3, nil, nil, 0);
	SinOsc(440, 0) * EnvGen(trg, 1, 0, 1, 0, env.asArray)

Trigger EnvGen with a pulse wave. MouseX controls trigger frequency. MouseY controls sustain time.

	var gate = LfPulse(MouseX(0.2, 20, 1, 0.2) ,0, MouseY(0.01, 0.99, 0, 0.2));
	var env = Env([0, 0.1, 0.1, 0], [0.001, 0.2, 0.3], -4, 2, nil, 0);
	SinOsc(440, 0) * EnvGen(gate, 1, 0, 1, 0, env.asArray)
