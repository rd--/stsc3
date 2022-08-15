# ToggleFF - toggle flip flop

_ToggleFF(trig)_

Toggles between zero and one upon receiving a trigger.

- trig: trigger input

Increasing density triggers frequency switcher:

	SinOsc((ToggleFF(Dust(XLn(1, 1000, 60))) * 400) + 800, 0) * 0.1
