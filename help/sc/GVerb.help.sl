# GVerb -- two-channel reverb

A two-channel reverb Ugen, based on the _GVerb_ Ladspa effect by Juhana Sadeharju.

_GVerb(in, roomsize, revtime, damping, inputbw, spread, drylevel, earlyreflevel, taillevel, maxroomsize)_

- in: mono input.
- roomsize: in squared meters.
- revtime: in seconds.
- damping: high frequency rolloff, 0 damps completely, 1 not at all.
- inputbw: same as damping control, but on the input signal.
- spread: a control on the stereo spread and diffusion of the reverb signal.
- drylevel: amount of dry signal.
- earlyreflevel: amount of early reflection level.
- taillevel: amount of tail level.
- maxroomsize: to set the size of the delay lines. Defaults to roomsize + 1.

Stereo decayed impulses, reverberated:

	var i = Impulse([1, 2], 0);
	var c = LfCub([900, 1200], 0);
	var s = Decay(i, [0.05, 0.25]) * c * 0.05;
	GVerb(s, 10, 3, 0.5, 0.5, 15, 1, 0.7, 0.5, 300).sum
