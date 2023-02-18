# LocalIn -- define and read from an audio bus local to the enclosing synth

_LocalIn(numChannels)_

- numChannels: the number of channels (i.e. adjacent buses) to read

LocalIn defines buses that are local to the enclosing synth.
These are like the global buses, but are more convenient if you want to implement a self contained effect that uses a feedback processing loop.

There can only be one audio rate and one control rate _LocalIn_ per synthesis program.

The audio can be written to the bus using _LocalOut_.

Audio written to a LocalOut will not be read by a corresponding LocalIn until the next cycle, i.e. one block size of samples later.

Simple delay with reversed channels sent to _LocalOut_ to give ping pong effect, mouse control of decay factor:

```
var source = Decay(Impulse(0.3, 0), 0.1) * WhiteNoise() * 0.2;
(* read feedback, add to source *)
var local = LocalIn(2, 0) + [source, 0];
var delayed = DelayN(local, 0.2, 0.2);
delayed <! LocalOut(delayed.reversed * MouseX(0, 1, 0, 0.2))
```
