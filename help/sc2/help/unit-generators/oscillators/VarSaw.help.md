# VarSaw - variable duty saw

_VarSaw(freq, iphase,width)_

- freq: frequency in Hertz
- iphase: initial phase offset in cycles ( 0..1 )
- width: duty cycle from zero to one.

Modulate frequency and width:

	VarSaw(
		LFPulse([3, 3.03], 0, 0.3) * 200 + 200, // frequency
		0, // initial phase
		LFTri(1, 0) * 0.5 + 0.5 // width
	) * 0.1 // mul

Same but with static width:

	VarSaw(
		LFPulse([3, 3.03], 0, 0.3) * 200 + 200, // frequency
		0, // initial phase
		0.2 // width
	) * 0.1 // mul

Compare VarSaw and LFPulse:

	[
		LFPulse(LFPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1,
		VarSaw(LFPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1
	]
