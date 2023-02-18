# SyncSaw -- hard sync sawtooth wave

_SyncSaw(ksyncFreq, ksawFreq)_

A sawtooth wave that is hard synched to a fundamental pitch. This produces an effect similar to moving formants or pulse width modulation. The sawtooth oscillator has its phase reset when the sync oscillator completes a cycle. This is not a band limited waveform, so it may alias.

- ksyncFreq: frequency of the fundamental.
- ksawFreq: frequency of the slave synched sawtooth wave. sawFreq should always be greater than syncFreq.

Modulate saw frequency:

	SyncSaw(100, Ln(100, 800, 12)) * 0.1

Modulate saw frequency with mouse (view is oscilloscope, _Impulse_ shows sync frequency):

	 var freq = 400;
	 [
		 SyncSaw(freq, freq * MouseX(1, 3, 0, 0.2)),
		 Impulse(freq, 0)
	 ] * 0.1

