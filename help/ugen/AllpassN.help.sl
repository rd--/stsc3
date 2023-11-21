(* AllpassN ; simple reverb ; https://listarc.cal.bham.ac.uk/lists/sc-users-2005/msg05902.html *)
var sinInput = SinOsc(LfNoise2(1).LinLin(-1, 1, 333, 555), 0);
var env = LfNoise2(1).Abs * 0.1;
var noiseInput = Dust(1).Decay(0.2) * PinkNoise() / 2;
var decayTime = 1;
var numDelays = 6;
var maxDelayTime = 0.05;
var output = sinInput * env + noiseInput;
numDelays.timesRepeat {
	output := AllpassN(
                output,
                maxDelayTime,
		{ Rand(0, maxDelayTime) } ! 2,
                decayTime
	)
};
output
