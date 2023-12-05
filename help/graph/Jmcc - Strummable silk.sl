(* strummable silk (jmcc) #11 *)
var x = MouseX(0, 1, 0, 0.2);
var str = (1 .. 8).collect { :ix |
	var n = 15;
	var tr = Hpz1(x > (0.25 + (ix - 1 * 0.07))).Abs;
	var env = Decay(Impulse(14, 0) * Lag(Trig(tr, 1), 0.2) * 0.01, 0.04);
	var pluck = PinkNoise() * env;
	var freq = ([-2 0 3 5 7 10 12 15][ix] + 60).MidiCps;
	var metal = RingzBank(
		pluck,
		(1 .. n).collect { :j | j * freq },
		[1],
		{ Rand(0.3, 1) } ! n
	);
	EqPan(metal, ix - 1 * 0.2 - 0.5)
};
var s = LeakDc(Lpf(str.Sum, 12000), 0.995);
6.timesRepeat {
	s := AllpassN(s, 0.1, { 0.05.Rand } ! 2, 4)
};
s
