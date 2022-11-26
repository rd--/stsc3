;; sawed cymbals (jmcc) #9
var sustainTime = 4;
var transitionTime = 4;
var dur = transitionTime * 2 + sustainTime;
{
	var p = 15; (* number of partials per channel per 'cymbal' *)
	var f1 = 500 + 2000.0.rand;
	var f2 = 8000.0.rand;
	var frequencies = { f1 + f2.rand } ! p;
	var ringTimes = { 2 + 4.0.rand } ! p;
	var osc = LFSaw(XLn(600.0.rand, 600.0.rand, dur), 0) * 0.0005;
	RingzBank(osc, frequencies, nil, ringTimes)
}.overlap(sustainTime, transitionTime, 6)
