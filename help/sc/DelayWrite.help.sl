# DelayWrite, DelayTap

_DelayWrite(buffer, in)_,
_DelayTap(buffer, delayTime)_

These unit generators implement delay line reading and writing in separate objects. This lets you put processing in the feedback loop, or granulate a delay line, or implement a ping pong delay or other feedback network. The Tap unit generators read from the delay line and DelayWr writes to it. You must supply an instance of Signal long enough to hold the maximum delay time you will require. You do not need to initialize the buffer.  The maximum delay time is the length of the buffer minus the block size. The minimum delay time is equal to the block size + 1.  A single delay line may have any number of Taps but only one DelayWr. The same buffer should be supplied to the DelayWr and all Tap unit generators which are part of the same delay line.

 TapN uses no interpolation, TapL uses linear interpolation, TapA uses all pass interpolation.

The output of DelayWr is just its input. The output of DelayWr is usually not needed, but it must be in the call graph of the Synth. In order to acheive this you will usually use the _<!_ operator which returns the first argument but ignores the second. This is just a bit of prestidigitation to give the DelayWr object an order in the call graph. Otherwise, if the Synth object is unable to trace up the graph and find theDelayWr object, it will never get called and the Taps will produce either garbage or silence. The use of _<!_ is shown below. Also see the help for _<!_.

DelayWr arguments:

- buffer: an instance of Signal.
- in:  the input signal to write to the delay line.

Tap arguments:

- delaytime: delay time in seconds.

Simple feedback delay (if this is all you want, Comb is easier to use):

	var buffer = BufAlloc(1, 48000 * 0.3).clearBuf; (* allocate a buffer for the delay line *)
	var input = Decay(Impulse(1, 0), 0.2) * PinkNoise(); (* make a percussive sound as input *)
	var delayedSignal = DelayTap(buffer, 0.15); (* tap the delay line at 0.15 second delay *)
	var mixedSignal = (delayedSignal * 0.4) + input; (* mix the delayed signal with the input *)
	var writer = DelayWrite(buffer, mixedSignal); (* write the mixed signal to the delay line *)
	mixedSignal <! writer (* output the mixed signal *)

Ping pong delay:

	var leftBuffer  = BufAlloc(1, 48000 * 0.4).clearBuf; (* allocate a buffer for the left delay line *)
	var rightBuffer  = BufAlloc(1, 48000 * 0.4).clearBuf; (* allocate a buffer for the right delay line *)
	var input = Decay(Impulse(0.4, 0), 0.1) * PinkNoise(); (* make a percussive sound as input *)
	var leftDelayedSignal = DelayTap(leftBuffer, 0.3); (* tap the left delay line *)
	var rightDelayedSignal = DelayTap(rightBuffer, 0.3); (* tap the left delay line *)
	var output = [leftDelayedSignal + input, rightDelayedSignal]; (* mix the delayed signal with the input *)
	var writer = DelayWrite([rightBuffer, leftBuffer], output * 0.8); (* feedback to buffers in reverse order *)
	output <! writer  (* output the mixed signal and force the DelayWr into the call graph *)

Distortion in the feedback loop:

	var buffer = BufAlloc(1, 48000 * 0.3).clearBuf; (* allocate a buffer for the delay line *)
	var input = FSinOsc(1000, 0) * LfPulse(0.3, 0, 0.05) * 0.3; (* sine pulse *)
	var delayedSignal = DelayTap(buffer, 0.15).Distort; (* tap the delay line at 0.15 second delay and distort *)
	var mixedSignal = (delayedSignal * 0.8) + input; (* mix the delayed signal with the input *)
	var writer = DelayWrite(buffer, mixedSignal); (* write the mixed signal to the delay line *)
	mixedSignal <! writer (* output the mixed signal *)

Pitch shift in the feedback loop:

	var buffer = BufAlloc(1, 48000 * 0.3).clearBuf; (* allocate a buffer for the delay line *)
	var input = FSinOsc(1000, 0) * LfPulse(0.3, 0, 0.05) * 0.3; (* sine pulse *)
	var delayedSignal = DelayTap(buffer, 0.15); (* tap the delay line at 0.15 seconds *)
	var shiftedSignal = PitchShift(delayedSignal, 0.2, 5 / 7, 0.01, 0.01); (* apply pitch shift *)
	var mixedSignal = (shiftedSignal * 0.8) + input; (* mix the delayed signal with the input *)
	var writer = DelayWrite(buffer, mixedSignal); (* write the mixed signal to the delay line *)
	mixedSignal <! writer (* output the mixed signal *)

