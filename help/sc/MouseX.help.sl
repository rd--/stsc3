# MouseX -- cursor unit generator

_MouseX(minval=0, maxval=1, warp=0, lag=0.2)_

- minval, maxval: range between left and right end of screen
- warp: mapping curve. 0 is linear, 1 is exponential (for freq or times e.g)
- lag: lag factor to dezipper cursor movement. (Default 0.2)

Mouse control of frequency:

	SinOsc(MouseX(40, 10000, 1, 0.2), 0) * 0.1

Same as above but with a two second lag:

	SinOsc(MouseX(40, 10000, 1, 2), 0) * 0.1

Two oscillators:

	SinOsc(
		[
			MouseX(40, 10000, 1, 0.2),
			MouseY(40, 10000, 1, 0.2)
		],
		0
	) * 0.1

* * *

See also: _MouseY_, _MouseButton_
