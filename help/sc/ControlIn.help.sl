# ControlIn - read bus

Control signals may be written to buses by Ugens, or they may be set by the client and expected to hold steady.
Therefore _ControlIn_ does not distinguish between new and old data.

_ControlIn_ always reads the current value on the bus, whether it was:

1. generated earlier in this calculation cycle
2. left over from the previous calculation cycle
3. set by the client

* * *

See: _AudioIn_, _ControlOut_, _In_, _Out_
