# VarSaw - variable duty saw

_VarSaw(freq, iphase,width)_

- freq: frequency in Hertz
- iphase: initial phase offset in cycles ( 0..1 )
- width: duty cycle from zero to one.

Modulate frequency and width:

	VarSaw(
		freq: LFPulse([3, 3.03], 0, 0.3) * 200 + 200,
		iphase: 0,
		width: LFTri(1, 0) * 0.5 + 0.5
	) * 0.1

Same but with static width:

	VarSaw(
		freq: LFPulse([3, 3.03], 0, 0.3) * 200 + 200,
		iphase: 0,
		width: 0.2
	) * 0.1

Compare VarSaw and LFPulse:

	[
		LFPulse(LFPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1,
		VarSaw(LFPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1
	]
