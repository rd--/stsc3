# PanAz -- azimuth panner

_PanAz(numChans, in, pos, level, width, orientation)_

Two channel equal power panner.

- numChans: number of output channels
- in: input signal
- pos: pan position. Channels are evenly spaced over a cyclic period of 2.0 in pos with 0.0 equal to channel zero and 2.0/numChans equal to channel 1, 4.0/numChans equal to channel 2, etc. Thus all channels will be cyclically panned through if a sawtooth wave from -1 to +1 is used to
modulate the pos.
- level: a control rate level input.
- width: The width of the panning envelope. Nominally this is 2.0 which pans between pairs of adjacent speakers. Width values greater than two will spread the pan over greater numbers of speakers. Width values less than one will leave silent gaps between speakers.

Five channel circular panning:

	PanAz(
		numChans: 5,
		in: ClipNoise(),
		pos: LfSaw(MouseX(0.2, 8, 1, 0.2), 0),
		level: 0.1,
		width: 3,
		orientation: 0.5
	)
