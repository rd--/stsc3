# Delay1 -- single sample delay

_Delay1(in)_

- in: sample to be delayed.

Original, with delayed subtracted from original:

	var z = Dust(1000);
	[z, z - Delay1(z)]

