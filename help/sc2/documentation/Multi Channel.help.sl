# Multi Channel Expansion

Multiple channels of audio are represented as Arrays.

	Blip(200, 8) + (* one channel, summed with *)
	[WhiteNoise(), Blip(403, 4)] * (* two channels, scaled by *)
	0.1

Each channel sent to Synth.play will go out a different speaker, so your limit here is two for a stereo output. If you have a supported multi channel sound card then you can output as many channels as the card supports.

Some Ugens create arrays of outputs. The _EqPan2_ Ugen returns an array of OutputProxys in order to implement multichannel output. An OutputProxy is just a place holder for an output of a multichannel Ugen.

	EqPan2(PinkNoise(), SinOsc(1, 0)) * 0.1

When an array is given as an input to a unit generator it causes an array of multiple copies of that unit generator to be made, each with a different value from the input array. This is called multi channel expansion. All but a few special unit generators perform multi channel expansion.  Only Arrays are expanded, no other type of Collection, not even subclasses of Array.

	Blip(500, 8) * 0.1 (* one channel *)

The array in the freq input causes an Array of 2 Blips to be created:

	Blip([499, 600], 8) * 0.1 (* two channels *)

Multi channel expansion will propagate through the expression graph.  When a unit generator constructor is called with an array of inputs, it returns an array of instances. If that array is the input to another constructor, then another array is created, and so on.

	Rlpf(Saw([100, 250]) * 0.05, XLn(8000, 400, 5), 0.05)

The _[100,250]_ array of frequency inputs to _Saw_ causes Saw to return an array of two Saws, that array causes _Rlpf_ to create two Rlpfs.  Both Rlpfs share a single instance of _XLn_.

When a constructor is parameterized by two or more arrays, then the number of channels created is equal to the longest array, with parameters being pulled from each array in parallel.  The shorter arrays will wrap.

For example, the following:

	Pulse([400, 500, 600], [0.5, 0.1]) * 0.1

is equivalent to:

	[Pulse(400, 0.5), Pulse(500, 0.1), Pulse(600, 0.5) ] * 0.1

A more complex example based on the Saw example above is given below.  In this example, the XLn is expanded to two instances, one going from 8000 Hz to 400 Hz and the other going in the opposite direction from 500 Hz to 7000 Hz. These two XLns are 'married' to the two Saw oscillators and used to parameterize two copies of Rlpf. So on the left channel a 100 Hz Saw is filtered from 8000 Hz to 400 Hz and on the right channel a 250 Hz Saw is filtered from 500 Hz to 7000 Hz.

	Rlpf(Saw([100, 250]) * 0.05, XLn([8000, 500], [400, 7000], 5), 0.05)

## Protecting arrays against expansion

Some unit generators such as Klank require arrays of values as inputs. Since all arrays are expanded, you need to protect some arrays by a Ref object. A Ref instance is an object with a single slot named 'value' that serves as a holder of an object. Ref.new(object) one way to create a Ref, but there is a syntactic shortcut.  The backquote ` is a unary operator that is equivalent to calling Ref.new(something).  So to protect arrays that are inputs to a Klank or similar Ugens you write:

	// ...

You can still create multiple Klanks by giving it an array of Ref'ed arrays.

	// ...

is equivalent to:

	// ...

## Reducing channel expansion with Mix

_sum_ provides the means for reducing multi channel arrays to a single channel. _[a, b, c].sum_ is equivalent to _a + b + c_.

Mix is more efficient than using + since it can perform multiple additions at a time.  But the main advantage is that it can deal with situations where the number of channels is arbitrary or determined at runtime.

	Pulse([400, 501, 600], [0.5, 0.1]).sum * 0.1 (* three channels of Pulse are mixed to one channel *)

Multi channel expansion works differently for Mix. Mix takes one input which is an array (one not protected by a Ref). That array does not cause copies of Mix to be made. All elements of the array are mixed together in a single Mix object.  On the other hand if the array contains one or more arrays then multi channel expansion is performed one level down. This allows you to mix an array of stereo (two element) arrays resulting in one two channel array. For example:

	// ...

is equivalent to:

	// ...

Currently it is not recursive. You cannot use Mix on arrays of arrays of arrays.

Here's a final example illustrating multi channel expansion and Mix.  By changing the variable 'n' you can change the number of voices in the patch. How many voices can your machine handle?

	var n = 8 * 12; (* number of 'voices' *)
	EqPan2( (* pan the voice to a stereo position *)
		CombL( (* a comb filter used as a string resonator *)
			Dust( (* random impulses as an excitation function *)
				{ Rand(0.9, 1.1) } ! n (* array expands Dust to n channels, one impulse per second on average *)
			) * 0.3, (* amplitude *)
			0.01, (* max delay time in seconds *)
			{ Rand(0.0003, 0.004) } ! n, (* array of different random lengths for each 'string' *)
			4 (* decay time in seconds *)
		),
		{ 1.Rand2 } ! n (* give each voice a different pan position *)
	).sum
