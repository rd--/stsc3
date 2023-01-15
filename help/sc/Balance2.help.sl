# Balance2 -- panning

_Balance2_ is a stereo signal balancer.

_Balance2(left, right, pos, level)_

Equal power panning balances two channels. By panning from left (pos=-1) to right (pos=1) you are decrementing the level of the left channel from 1 to 0 taking the square root of the linear scaling factor, while at the same time incrementing the level of the right channel from 0 to 1 using the same curve. In the center position (pos=0) this results in a level for both channels of 0.5.sqrt (~=0.707 or -3dB). The output of _Balance2_ remains a stereo signal.

Balance2 places the left input in the left channel and the right input in the right channel and applies independent multipliers to each channel.
When _pos_ is -1 only the left channel is heard, the multipliers are 1 and 0, i.e. × ⟦1, 0⟧
When _pos_ is +1 only the right channel is heard, the multipliers are 0 and 1, i.e. × ⟦0, 1⟧
When _pos_ is 0 the two channels are balanced and both multipliers are √½ (≈ 0.7), i.e. × ⟦√½, √½⟧

In this program two sine oscillators (at 440 and 550 hz) are balanced using a low frequency noise generator.

```
var o = SinOsc([440, 550], 0);
Balance2(o.first, o.second, LfNoise1(4), 0.1)
```
