# WrapIndex -- index into a table with a signal

_WrapIndex(table, in)_

The input signal value is truncated to an integer value and used as an index into the table. Out of range index values are wrapped cyclically to the valid range.

- table: an instance of FloatArray or Signal.
- in: the input signal.

Indexing into a table:

	var buf = [200, 300, 400, 500, 600, 800].asLocalBuf;
	SinOsc(WrapIndex(buf, MouseX(0, 6 * 3, 0, 0.1)), 0) * 0.1
