# ToggleFf - toggle flip flop

_ToggleFf(trig)_

Toggles between zero and one upon receiving a trigger.

- trig: trigger input

Increasing density triggers frequency switcher:

	SinOsc((ToggleFf(Dust(XLn(1, 1000, 60))) * 400) + 800, 0) * 0.1
