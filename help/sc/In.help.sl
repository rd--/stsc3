# In -- read from bus

Read a signal from a bus.

- _In(numChannels, bus)_

- bus: the index of the bus to read in from
- numChannels: the number of adjacent buses to read

_In_ reads signals from consecutive buses.
Audio and control buses behave differently with respect to signals left on the bus in the previous calculation cycle.

_In_ can access audio signals that were generated in the current calculation cycle by synthesis nodes located earlier in the node tree.
It does not read signals left on an audio bus from the previous calculation cycle.
_InFeedback_ supports audio signal feedback.

The hardware input busses begin just after the hardware output busses.
The number of hardware input and output busses can vary depending on the synthesiser parameters.

Read and delay the first two hardware input channels:

```
DelayN(In(2, NumOutputBuses()), 2, 2)
```

Write and then read from an audio bus:

```
Out(10, PinkNoise() * 0.1) <! Out(0, In(1, 10))
```

One two channel _In_, delayed by one second:

```
DelayN(In(2, NumOutputBuses()), 1, 1)
```

Two single channel _In_ nodes, each delayed by one second:

```
DelayN(In(1, NumOutputBuses() + [0, 1]), 1, 1)
```

* * *

See also: _AudioIn_, _ControlIn_, _InFeedback_, _Out_
