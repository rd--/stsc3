# Pitch -- autocorrelation pitch follower

_Pitch(in, initFreq, minFreq, maxFreq, execFreq, maxBinsPerOctave, median, ampThreshold, peakThreshold, downSample)_

Returns two signals, _freq_ and _hasFreq_.

This is a better pitch follower than ZeroCrossing, but more costly of CPU. For most purposes the default settings can be used and only in needs to be supplied. Pitch returns two values (via an Array of OutputProxys, see the OutputProxy help file), a freq which is the pitch estimate and hasFreq, which tells whether a pitch was found. Some vowels are still problematic, for instance a wide open mouth sound somewhere between a low pitched short 'a' sound as in 'sat', and long 'i' sound as in 'fire', contains enough overtone energy to confuse the algorithm.

Track sine oscillator:

	var x = MouseX(220, 660, 0, 0.1);
	var y = MouseY(0.01, 0.1, 0, 0.1);
	var z = SinOsc(x, 0) * y;
	var f = Pitch(z, 440, 60, 4000, 100, 16, 7, 0.02, 0.5, 1, 0);
	[z, SinOsc(f.first / 2, 0) * 0.1]

Track audio input, **use headphones**:

	var z = AudioIn([1, 2]).sum;
	var f = Pitch(z, 440, 60, 4000, 100, 16, 7, 0.02, 0.5, 1, 0);
	var s = VarSaw(
		f.first * [0.5, 1, 2],
		0,
		LfNoise1(0.3) * 0.1 + 0.1
	) * 0.1;
	6.timesRepeat {
		s := AllpassN(s, 0.040, { Rand(0, 0.04) } ! 2, 2)
	};
	s

## How it works

The pitch follower executes periodically at the rate specified by execFreq in cps. execFreq is clipped to be between minFreq and maxFreq . First it detects whether the input peak to peak amplitude is above the ampThreshold. If it is not then no pitch estimation is performed, hasFreq is set to zero and freq is held at its previous value. It performs an autocorrelation on the input and looks for the first peak after the peak around the lag of zero that is above peakThreshold times the amplitude of the peak at lag zero.

Using a peakThreshold of one half does a pretty good job of eliminating overtones, and finding the first peak above that threshold rather than the absolute maximum peak does a good job of eliminating estimates that are actually multiple periods of the wave.

The autocorrelation is done coarsely at first using a maximum of maxBinsPerOctave lags until the peak is located. Then a fine resolution search is performed until the peak is found. (Note that maxBinsPerOctave does NOT affect the final pitch resolution; a fine resolution search is always performed. Setting maxBinsPerOctave larger will cause the coarse search to take longer, and setting it smaller will cause the fine search to take longer.)

The three values around the peak are used to find a fractional lag value for the pitch. If the pitch frequency is higher than maxFreq, or if no peak is found above minFreq, then hasFreq is set to zero and freq is held at its previous value.

It is possible to put a median filter of length median on the output estimation so that outliers and jitter can be eliminated. This will however add latency to the pitch estimation for new pitches, because the median filter will have to become half filled with new values before the new one becomes the median value. If median is set to one then that is equivalent to no filter, which is the default.

When an in range peak is found, it is inserted into the median filter, a new pitch is read out of the median filter and output as freq, and hasFreq is set to one.

It is possible to down sample the input signal by an integer factor downSample in order to reduce CPU overhead. This will also reduce the pitch resolution.

Until Pitch finds a pitch for the first time, it will output initFreq.

None of these settings are time variable.

Default Argument values: _initFreq_ = 440.0 _minFreq_ = 60.0 _maxFreq_ = 4000.0 _execFreq_ = 100.0 _maxBinsPerOctave_ = 16 _median_ = 1 _ampThreshold_ = 0.01 _peakThreshold_ = 0.5 _downSample_ = 1

