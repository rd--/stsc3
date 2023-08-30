(* ---- MiClouds ; requires=keywords ; requires>#14 *)
var pit = LfNoise1(0.3) * 12;
var pos = LfNoise2(0.4) * 0.5 + 0.5;
var size = LfNoise1(0.3) * 0.5 + 0.5;
var dens = LfNoise1(0.3) * 0.5 + 0.5;
var tex = LfNoise1(0.3) * 0.5 + 0.5;
var freeze = LfClipNoise(0.3);
var tr = Dust([0.8, 1.1]);
var freq = Latch(PinkNoise() * 24 + 80, tr).MidiCps;
var inputArray = Rlpf(tr, freq, 0.002) * 4;
MiClouds(
	pit: pit,
	pos: pos,
	size: size,
	dens: dens,
	tex: tex,
	drywet: 0.5,
	inGain: 2,
	spread: 0.5,
	rvb: 0.3,
	fb: 0.8,
	freeze: freeze,
	mode: 0,
	lofi: 1,
	trig: 0,
	inputArray: inputArray
)
