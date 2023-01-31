;; 20061015 ; discretion ; rd ; requires=EnvBreakPoint
var mkls = { :ts :bp |
	EnvGen(1, 1, 0, ts, 2, EnvBreakPoint(bp, 0).asArray)
};
var part = {
	var ts = 21;
	var f = mkls(ts, [{ Rand(50, 55) } ! 2, 0.33, { Rand(50, 65) } ! 2, 1, { Rand(50, 55) } ! 2]);
        var g = mkls(ts, [0, 0.33, { Rand(0.01, 0.035) } ! 2, 1, 0]);
	Saw(f) * g
};
part !+ 8
