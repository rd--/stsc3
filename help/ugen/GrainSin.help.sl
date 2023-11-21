(* GrainSin *)
var nc = system.scSynth.mainOutputs;
var overlap = MouseY(0, 2, 0, 0.2);
var f = MouseX(1, 220, 0, 0.2);
var t = Impulse(f, 0);
var a = GrainSin(nc, t, overlap / f, 440, Rand(t, -1, 1), -1, 512);
LeakDc(a, 0.995) * 0.1

(* GrainSin ; f0 https://www.listarc.bham.ac.uk/lists/sc-users/msg66911.html ; requires=kr *)
var nc = system.scSynth.mainOutputs;
var t = Impulse(MouseY(1, 999, 0, 0.2), 0);
var l = (0 .. 15).collect { :i |
	i ^ LfNoise0(1).Range(0.3, 0.7) + 1 * 99
};
GrainSin(nc, t, MouseX(0.001, 0.5, 1, 0.2), Choose(t, l.kr), Rand(t, -1, 1), -1, 512) * 0.1
