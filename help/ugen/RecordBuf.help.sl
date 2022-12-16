;; RecordBuf ; simple delay line ; reader is PlayBuf and writer is RecordBuf
var buffer = BufAlloc(1, 48000 * 0.3).BufClear; (* allocate and clear a buffer for the delay line *)
var input = Decay(Impulse(1, 0), 0.2) * PinkNoise(); (* make a percussive sound as input *)
var delayedSignal = PlayBuf(1, buffer, 1, 1, 0.15, 1, 0); (* tap the delay line at 0.15 second delay *)
var mixedSignal = (delayedSignal * 0.4) + input; (* mix the delayed signal with the input *)
var writer = RecordBuf(buffer, 0, 1, 0, 1, 1, 0, 0, mixedSignal); (* write the mixed signal to the delay line *)
mixedSignal <! writer (* output the mixed signal *)
