# Index -- index into a table with a signal

_Index(table, in)_

The input signal value is truncated to an integer value and used as an index into the table. Out of range index values are clipped to the valid range.

- table: an instance of FloatArray or Signal.
- in: the input signal.

Index buffer for frequency values:

	var b = [50, 100, 200, 400, 800, 1600].asLocalBuf;
	var f = Index(b, LinLin(LfSaw(2, 0), -1, 1, 0, 6));
	SinOsc([f, f * 9], 0) * 0.1

