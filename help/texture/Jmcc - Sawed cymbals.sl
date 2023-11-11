(* sawed cymbals (jmcc) #9 *)
var sustainTime = 4;
var transitionTime = 4;
var dur = transitionTime * 2 + sustainTime;
{
	var p = 15; (* number of partials per channel per 'cymbal' *)
	var f1 = 500 + 2000.Rand;
	var f2 = 8000.Rand;
	var frequencies = { f1 + f2.Rand } ! p;
	var ringTimes = { 2 + 4.Rand } ! p;
	var osc = LfSaw(XLine(600.Rand, 600.Rand, dur), 0) * 0.0005;
	RingzBank(osc, frequencies, nil, ringTimes)
}.overlap(sustainTime, transitionTime, 6)
