# RoundTo -- round to multiple of

_RoundTo(self, aNumber=1)_

Quantization by rounding. Rounds _self_ to the nearest multiple of _aNumber_.

	var l = Ln(48, 57, 23);
	SinOsc([l, l.RoundTo(1)].MidiCps, 0) * 0.1

