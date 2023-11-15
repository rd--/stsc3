(* thx simulation (geirmund simonsen) *)
var freqArray = [77 74 72 70 65 62 60 58 53 50 46 34].MidiCps;
var randomFreq = Env([1 1 0.007], [8 6], [0 -4], 0, 0, 0).asEnvGen(1);
var ampEnv = Env([0.07 0.07 0.21], [8 6], [0 1], 0, 0, 0).asEnvGen(1);
freqArray.collect { :item |
	var freqEnv = Env([0 0 item], [8 6], [0 -3], 0, 0, 0).asEnvGen(1);
	var freq = LfNoise2(1.3) * 100 + 230 * randomFreq + freqEnv;
	EqPan2(Saw(freq), LfNoise2(1.3))
}.Mix * ampEnv * 0.55
