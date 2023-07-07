# Out -- summing output

- _Out(bus, channelsArray)_

Sum a sequence of signals to a bus.

- bus: the index of the bus to sum out to, the lowest numbers correspond to the audio hardware
- channelsArray: an array of channels to write out

One _Out_ summing one _SinOsc_ to two buses:

	Out(0, SinOsc(440, 0) * 0.1 ! 2)

Two _Out_ summing distinct frequencies to one bus each:

	Out(0, SinOsc(440, 0) * 0.1) <! Out(1, SinOsc(443, 0) * 0.1)

One _Out_ summing to two buses:

	Out(0, SinOsc([440, 443], 0) * 0.1)

* * *

See also: _In_, _AudioIn_, _NumOutputBuses_
