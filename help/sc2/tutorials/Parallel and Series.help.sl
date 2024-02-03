# 5. Parallel & Series - Programmatic Patch Construction

The ability to construct patches programmatically is one of the unique and powerful features of SuperCollider. It allows you to create structures whose size and complexity are determined at runtime.

## 5.1 How to build parallel structures

As discussed in the previous section Mix can be used to mix parallel structures.  We can use Array.fill to fill an Array with a number of variations of some structure which we mix together using Mix.

Mixing sine oscillators in parallel:

	let n = 16; {- number of structures to make -}
	let f = { FSinOsc(Rand(200, 1200), 0) }; {- function to create an oscillator at a random frequency -}
	{ f:/0 ! 2 } !> n / n * 0.1 {- stereo duplicate, array of n places, mixed, scale amplitude -}

Filling an Array and summing it is a common idiom, `{ ... }.duplicate(n).Sum`, which is implemented as the _!+_ operator.  The operator _!>_ uses _Mix_ in place of _Sum_.

One common structure used in reverbs is to mix several comb delays in parallel.  This example shows how you can use parallel structures to process a single input.

	let n = 8;
	{- variable to hold the input, use a noise burst as an input signal -}
	let z = Decay2( {- exponential decay envelope -}
		Impulse(0.5, 0), {- impulse to trigger the decay -}
		0.01, {- attackTime -}
		0.20 {- decayTime -}
	) * PinkNoise() * 0.1; {- multiply envelope by pink noise -}
	{
		{- function to create comb delays with random delay times -}
		CombC(
			z, {- input signal -}
			0.1, {- maximum delay time -}
			Rand(0.01, 0.09), {- random delay time from 0.01 to 0.09 seconds -}
			3 {- decaytime -}
		)
	} !+ n

# 5.2 How to build series structures.

Another structure used in reverbs is a series of allpass delays.  A series or chain of unit generators can be created by using a loop with an induction variable.  In the following loop the variable _z_ gets reassigned each time through the loop creating a chain of allpass delays. The first time though the loop the variable _z_ is the input signal generator.  _Z_ is then assigned to the allpass delay, so that the next time through the loop the next allpass delay will have the previous allpass delay as its input.

	let n = 8;
	let z = Decay2(
		Impulse(0.5, 0),
		0.01,
		0.20
	) * PinkNoise() * 0.1;
	{- z begins as the input and gets reassigned each time through the loop -}
	n.timesRepeat { {- function to create allpass delays with random delay times -}
		z := AllpassN(
			z, {- input to this allpass -}
			0.05, {- maximum delay time -}
			Rand(0, 0.05), {- random delay time -}
			3 {- echo decay time -}
		)
	};
	z {- return z as the output -}

# 5.3 Conditional patch construction

Patches can be constructed conditionally at runtime.
The following example uses conditional code to randomly construct a patch each time.

	{- frequency control -}
	let freq = [LfNoise0(3), FSinOsc(3, 0)].atRandom * 500 + 800;
	{- amplitude control -}
	let amp = [LfNoise1(2.2).Max(0) * 0.5, FSinOsc(2.2, 0) * 0.25 + 0.25].atRandom;
	{- sound source -}
	[SinOsc(freq, 0), Blip(freq, 8)].atRandom * amp * 0.2
