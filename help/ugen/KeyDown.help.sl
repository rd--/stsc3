(* Event control ; Kyma keyboard names *)
(1 .. 16).collect { :vc |
	SinOsc(KeyPitch(vc).UnitCps, 0) * KeyDown(vc) * KeyVelocity(vc)
}.Splay2

(* Event control ; Kyma keyboard names *)
(1 .. 16).collect { :vc |
	var freq = KeyPitch(vc).UnitCps + LfNoise2(0.25);
	var amp = Asr(KeyDown(vc), 0.1, 2, -4) * KeyVelocity(vc); (* Latch(KeyVelocity(vc), KeyDown(vc)) *)
	Lpf(Saw(freq), KeyTimbre(vc) + 0.1 * freq * 8) * amp
}.Splay2
