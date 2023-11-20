(* AllpassN ; simple reverberator ; https://listarc.cal.bham.ac.uk/lists/sc-users-2005/msg05902.html *)
var input = Dust(1).Decay(0.2) * PinkNoise();
var decayTime = 1;
var numDelays = 6;
var maxDelayTime = 0.05;
var output = input;
numDelays.timesRepeat {
	output := AllpassN(
                output,
                maxDelayTime,
		{ Rand(0, maxDelayTime) } ! 2,
                decayTime
	)
};
output
