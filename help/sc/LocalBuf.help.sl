# LocalBuf -- buffer

Allocate a buffer local to the synth

- _LocalBuf(numChannels, numFrames)_
- _LocalBuf(anArray)_

- numChannels: number of channels for multiple channel buffers (default: 1)
- numFrames: number of frames (default: 1)

_LocalBuf_ outputs a buffer number.
Most but not all Ugens that require a buffer number input accept local buffers.

The second constructor makes a local buffer from an array of numbers.
