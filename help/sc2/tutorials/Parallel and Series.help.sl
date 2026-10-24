# 5. Parallel & Series - Programmatic Patch Construction

The ability to construct patches programmatically is one of the unique and powerful features of SuperCollider. It allows you to create structures whose size and complexity are determined at runtime.

## 5.1 How to build parallel structures

As discussed in the previous section Mix can be used to mix parallel structures.  We can use Array.fill to fill an Array with a number of variations of some structure which we mix together using Mix.

Mixing sine oscillators in parallel:

~~~spl
/* number of structures to make */
let n = 16;
/* function to create an oscillator at a random frequency */
let f = { FSinOsc(Rand(200, 1200), 0) };
/* stereo duplicate, array of n places, mixed, scale amplitude */
{ f/0 ! 2 } !> n / n * 0.1
~~~

Filling an Array and summing it is a common idiom, `{ ... }.duplicate(n).Sum`, which is implemented as the _!+_ operator.  The operator _!>_ uses _Mix_ in place of _Sum_.

One common structure used in reverbs is to mix several comb delays in parallel.  This example shows how you can use parallel structures to process a single input.

~~~spl
let n = 8;
/* variable to hold the input, use a noise burst as an input signal */
let z = (
	/* exponential decay envelope */
	Decay2(
		/* impulse to trigger the decay */
		Impulse(0.5, 0),
		/* attackTime */
		0.01,
		/* decayTime */
		0.20
	)
	/* multiply envelope by pink noise */
	* PinkNoise() * 0.1
);
/* function to create comb delays with random delay times */
{
	CombC(
		/* input signal */
		z,
		/* maximum delay time */
		0.1,
		/* random delay time from 0.01 to 0.09 seconds */
		Rand(0.01, 0.09),
		/* decaytime */
		3
	)
} !+ n
~~~

# 5.2 How to build series structures.

Another structure used in reverbs is a series of allpass delays.  A series or chain of unit generators can be created by using a loop with an induction variable.  In the following loop the variable _z_ gets reassigned each time through the loop creating a chain of allpass delays. The first time though the loop the variable _z_ is the input signal generator.  _Z_ is then assigned to the allpass delay, so that the next time through the loop the next allpass delay will have the previous allpass delay as its input.

~~~spl
let n = 8;
let z = Decay2(
	Impulse(0.5, 0),
	0.01,
	0.20
) * PinkNoise() * 0.1;
/* z begins as the input and gets reassigned each time through the loop */
n.timesRepeat {
	/* function to create allpass delays with random delay times */
	z := AllpassN(
		/* input to this allpass */
		z,
		/* maximum delay time */
		0.05,
		/* random delay time */
		Rand(0, 0.05),
		/* echo decay time */
		3
	)
};
/* return z as the output */
z
~~~

# 5.3 Conditional patch construction

Patches can be constructed conditionally at runtime.
The following example uses conditional code to randomly construct a patch each time.

~~~spl
/* frequency control */
let freq = [
	LfNoise0(3),
	FSinOsc(3, 0)
].atRandom * 500 + 800;
/* amplitude control */
let amp = [
	LfNoise1(2.2).Max(0) * 0.5,
	FSinOsc(2.2, 0) * 0.25 + 0.25
].atRandom;
/* sound source */
[
	SinOsc(freq, 0),
	Blip(freq, 8)
].atRandom * amp * 0.2
~~~
