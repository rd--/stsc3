;; blips 001 (jmcc) #SC3d1.5
var blipProc = {
	Blip(
		XLn(ExpRand(0.25, 400), ExpRand(0.25, 400), 4),
		XLn(ExpRand(2, 100), ExpRand(2, 100), 4)
	)
};
{
	if(0.8.coin) {
		var z = Pan2(blipProc() * blipProc(), Ln(1.Rand2, 1.Rand2, 4), 0.3).Distort;
		6.timesRepeat { z := AllpassN(z, 0.05, { 0.05.Rand } ! 2, 4) };
		z
	} {
		Silent(2)
	}
}.overlap(2, 1, 12)
