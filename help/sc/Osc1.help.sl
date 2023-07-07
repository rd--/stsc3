# Osc1 -- one-shot oscillator

_Osc1(table, dur)_

An oscillator that reads through a table only once.

- table: an instance of Signal; its size must be a power of 2.
- dur: how long to read through the table

Pitch class table, linear interpolation, first slowly, then quickly, then slowly again:

	var tbl = [0, 2, 10, 12].asLocalBuf;
	SinOsc((Osc1(tbl, 5) + 48).MidiCps, 0) * 0.1

