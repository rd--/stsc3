# VarSaw -- variable duty saw

_VarSaw(freq, iphase,width)_

- freq: frequency in Hertz
- iphase: initial phase offset in cycles (0 .. 1)
- width: duty cycle from zero to one.

Modulate frequency and width:

	VarSaw(
		freq: LfPulse([3, 3.03], 0, 0.3) * 200 + 200,
		iphase: 0,
		width: LfTri(1, 0) * 0.5 + 0.5
	) * 0.1

Same but with static width:

	VarSaw(
		freq: LfPulse([3, 3.03], 0, 0.3) * 200 + 200,
		iphase: 0,
		width: 0.2
	) * 0.1

Compare VarSaw and LfPulse:

	[
		LfPulse(LfPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1,
		VarSaw(LfPulse(3, 0, 0.3) * 200 + 200, 0, 0.2) * 0.1
	]
