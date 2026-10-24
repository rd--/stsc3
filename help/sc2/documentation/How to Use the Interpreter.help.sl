# How to use the Interpreter

You can execute any single line expression by clicking anywhere in that line and pressing the 'Enter' key, which is the shortcut for the Evaluate Selection menu item.
Note that the 'enter' key is not the same key as 'return'.

~~~spl
SinOsc(800, 0).Times(0.1).play
~~~

In the help files all executable fragments are written in the Monaco font.

If an expression has multiple lines, then you need to select all of the lines before typing 'Enter'.

However, most examples in the manual have parentheses around the code which allows you to double click to the right of the open paren and select the entire expression.
Then press 'enter'.

~~~spl
/* number of strings */
let n = 5;
/* array of possible impulse excitation behaviours */
let a = [
	/* slow phasing */
	{ Impulse(2 + Rand(0, 0.2), 0) * 0.3 },
	/* wind chimes */
	{ Dust(0.5) * 0.3 },
	 /* races */
	{
		Impulse(
			SinOsc(
				0.05 + Rand(0, 0.1),
				2 * Rand(0, 1.pi)
			) * 5 + 5.2, 0
		) * 0.3
	}
];
/* choose one at random to use for all voices */
let b/0 = a.atRandom;
/* n strings tuned randomly to Midi keys 60-90 */
{
	/* calculate delay based on a random note */
	let noteNumber = 60 + IRand(0, 30);
	let delayTime = 1 / noteNumber.MidiCps;
	EqPan(
		/* used as a string resonator */
		CombL(
			/* decaying envelope for noise */
			Decay(
				/* instantiate an exciter */
				b(),
				/* decay time of excitation */
				0.04
			)
			/* multiply noise by envelope */
			* PinkNoise() * 0.2,
			/* max delay time */
			delayTime,
			/* actual delay time */
			delayTime,
			/* decay time of string */
			4
		),
		/* random pan position */
		Rand(-1, 1)
	)
} !+ n
~~~
