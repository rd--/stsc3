(* monastic resonance ; jmcc #5 ; mouse controls size and reverb time ; requires=AudioIn *)
var decayTime = MouseX(0, 16, 0, 0.2);
var delayScale = MouseY(0.01, 1, 0, 0.2);
var s = AudioIn([1, 2]) * 0.005;
var z = DelayC(s.Sum, 0.048, 0.048);
var y = CombL(z, 0.1, { Rand(0.01, 0.09) } ! 8 * delayScale, decayTime).Sum;
5.timesRepeat {
	y := AllpassC(y, 0.050, { Rand(0, 0.05) } ! 2, 1)
};
LeakDc(y, 0.995)
