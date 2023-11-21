(* CurveGen *)
var env = CurveGen(1, [0.5 2], [9], [-4]);
SinOsc(440 * env, 0) * 0.1

(* CurveGen ; https://audiomasher.org/patch/Y3OOFF *)
var tEnvx = { :tr :atk :hld :rel |
	CurveGen(tr, [0 1 1 0], [atk hld rel], [0 0 -9])
};
var noteZero = MouseY(48, 75, 0, 0.2).rounded;
var decayTime = MouseX(2, 4, 0, 0.2);
var seq = Dseq(inf, [0 5 0 3 0 3 5 0 3] + noteZero);
var tr = Impulse(4, 0);
var osc = SinOsc(Demand(tr, 0, seq.MidiCps), 0) * 0.1;
var env = tEnvx(tr, 0.01, 0.1, 0.2) + tEnvx(PulseDivider(tr, 2, 0), 0.01, 0.1, 0.2);
var snd = osc * env ! 2;
CombC(snd, 0.25, 0.25, decayTime) * 0.4 + snd
