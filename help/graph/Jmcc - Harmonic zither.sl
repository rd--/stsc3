(* harmonic zither (jmcc) #11 ; mouse control *)
var pitch = [50 53.86 57.02 59.69 62 64.04 65.86 67.51 69.02 71.69 72.88 74];
var triggerSpacing = 0.5 / (pitch.size - 1);
var panSpacing = 1.5 / (pitch.size - 1);
var strings = pitch.indices.collect { :i |
	var trigger = Hpz1(MouseX(0, 1, 0, 0.2) > (0.25 + i * triggerSpacing)).Abs;
	var pluck = PinkNoise() * Decay(trigger, 0.05);
	var period = pitch[i].MidiCps.Recip;
	var string = CombL(pluck, period, period, 8);
	EqPan(string, i - 1 * panSpacing - 0.75)
};
LeakDc(Lpf(strings.Sum, 12000), 0.995)
