# How to use the Interpreter

You can execute any single line expression by clicking anywhere in that line and pressing the 'Enter' key, which is the shortcut for the Evaluate Selection menu item.  Note that the 'enter' key is not the same key as 'return'.

	{ FSinOsc(800, 0) * 0.1 }.play

In the help files all executable fragments are written in the Monaco font.

If an expression has multiple lines, then you need to select all of the lines before typing 'Enter'.

However, most examples in the manual have parentheses around the code which allows you to double click to the right of the open paren and select the entire expression. Then press 'enter'.

	var n = 5; (* number of strings *)
	var b = [ (* array of possible impulse excitation behaviours *)
		{ Impulse(2 + 0.2.Rand, 0) * 0.3 }, (* slow phasing *)
		{ Dust(0.5) * 0.3 }, (* wind chimes *)
		{ Impulse(SinOsc(0.05 + 0.1.Rand, 2 * pi.Rand) * 5 + 5.2, 0) * 0.3 } (* races *)
	].atRandom; (* choose one at random to use for all voices *)
	{ (* n strings tuned randomly to MIDI keys 60-90 *)
		var delayTime = 1 / (60 + 30.IRand).MidiCps; (* calculate delay based on a random note *)
		Pan2(
			CombL( (* used as a string resonator *)
				Decay( (* decaying envelope for noise *)
					b(), (* instantiate an exciter *)
					0.04
				) (* decay time of excitation *)
				* PinkNoise() * 0.2, (* multiply noise by envelope *)
				delayTime,  (* max delay time *)
				delayTime, (* actual delay time *)
				4  (* decay time of string *)
			),
			1.Rand2, (* random pan position *)
			1 (* level *)
		)
	} !+ n
