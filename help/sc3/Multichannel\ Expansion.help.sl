# Multichannel Expansion

Explaining multichannel expansion and representation

## Multiple channels as Arrays

Multiple channels of audio are represented as Arrays.

One channel:

	Blip(800, 4) * 0.1

Two channels:

	[Blip(800, 4), PinkNoise()] * 0.1

Each channel of output will go out a different speaker, so your limit here is two for a stereo output.
If you have a supported multi channel audio interface or card then you can output as many channels as the card supports.

All UGens have only a single output.
This uniformity facilitates the use of array operations to perform manipulation of multi channel structures.

In order to implement multichannel output, UGens create a separate UGen known as an OutputProxy for each output.
An OutputProxy is just a place holder for the output of a multichannel UGen.
OutputProxies are created internally, you never need to create them yourself,
but it is good to be aware that they exist so you will know what they are when you run across them.

Look at the outputs of Pan2:

	Pan2(PinkNoise(), SinOsc(3, 0), 0.1)

## Multichannel expansion

When an Array is given as an input to a unit generator it causes an array of multiple copies of that unit generator to be made, each with a different value from the input array.
This is called multichannel expansion.
All but a few special unit generators perform multichannel expansion.
Only Arrays are expanded, no other type of Collection, not even subclasses of Array.

	Blip(500, 8) * 0.1

The array in the freq input causes an Array of 2 Blips to be created:

	Blip([499, 600], 8) * 0.1

Multichannel expansion will propagate through the expression graph.
When a unit generator constructor is called with an array of inputs, it returns an array of instances.
If that array is the input to another constructor, then another array is created, and so on.

	Rlpf(Saw([100, 250]) * 0.05, XLine(8000, 400, 5), 0.05)

The [100, 250] array of frequency inputs to Saw causes Saw.ar to return an array of two Saws,
that array causes Rlpf to create two Rlpfs.

Both Rlpf Ugens share a single instance of XLine.

When a constructor is parameterized by two or more arrays, then the number of channels created is equal to the longest array, with parameters being pulled from each array in parallel.
The shorter arrays will wrap.

For example, the following:

	Pulse([400, 500, 600], [0.5, 0.1]).Splay * 0.1

is equivalent to:

	[Pulse(400, 0.5), Pulse(500, 0.1), Pulse(600, 0.5)].Splay * 0.1

A more complex example based on the Saw example above is given below.
In this example, the XLine is expanded to two instances, one going from 8000 Hz to 400 Hz and the other going in the opposite direction from 500 Hz to 7000 Hz.
These two XLines are 'married' to the two Saw oscillators and used to parameterize two copies of RLPF.
So on the left channel a 100 Hz Saw is filtered from 8000 Hz to 400 Hz and on the right channel a 250 Hz Saw is filtered from 500 Hz to 7000 Hz.

	Rlpf(Saw([100, 250]), XLine([8000, 500],[400, 7000], 5), 0.05) * 0.1

## Expanding methods and operators

Many operators and methods also multichannel expand.
For example all common math operators, multiply:

	Saw([100, 250]) * [0.05, 0.08]

and addition:

	Saw(LfNoise1(1).Range(0, 100) + [100, 250]) * 0.1

Also the various Ugen convenience functions like Range:

	Saw(LfNoise1(1).Range(100, [200, 300])) * 0.1

and Lag:

	Saw(LfPulse(1, 0, 0.5).Range(100, [200, 300]).Lag([0.1, 0.5])) * 0.1

The expansion is handled by wrapper-methods defined in SequenceableCollection.

You can use Object: -multiChannelPerform to do multichannel expansion with any method on any kind of object:

	...

The shorter arrays wrap:

	...

## Using flop for multichannel expansion

The method flop swaps columns and rows, allowing to derive series of argument sets:

(
SynthDef("help_multichannel", { |out=0, freq=440, mod=0.1, modrange=20|
    Out.ar(out,
        SinOsc.ar(
            LFPar.kr(mod, 0, modrange) + freq
        ) * EnvGate(0.1)
    )
}).add;
)
(
var freq, mod, modrange;
​
freq = Array.exprand(8, 400, 5000);
mod = Array.exprand(8, 0.1, 2);
modrange = Array.rand(8, 0.1, 40);
​
fork {
    [\freq, freq, \mod, mod, \modrange, modrange].flop.do { |args|
        args.postln;
        Synth("help_multichannel", args);
        0.3.wait;
    }
};
)

Similarly, Function:flop returns an unevaluated function that will expand to its arguments when evaluated:

(
SynthDef("blip", { |out, freq|
    Out.ar(out,
        Line.ar(0.1, 0, 0.05, 1, 0, 2) * Pulse.ar(freq * [1, 1.02])
    )
}).add;
​
a = { |dur=1, x=1, n=10, freq=400|
    fork { n.do {
            if(x.coin) { Synth("blip", [\freq, freq]) };
            (dur / n).wait;
    } }
}.flop;
)
​
a.value(5, [0.3, 0.3, 0.2], [12, 32, 64], [1000, 710, 700]);

## Pitfalls

Consider Pan2 expanded and summed:

	Pan2(SinOsc([500, 600], 0), [-0.5, 0.5], 0.1).Sum

Compared to Pan2 expanded, transposed & summed:

	Pan2(SinOsc([500, 600], 0), [-0.5, 0.5], 0.1).transposed.Sum

## Protecting arrays against expansion

...

## Reducing channel expansion with Mix

The Mix object provides the means for reducing multichannel arrays to a single channel.

Mix is more efficient than using + since it can perform multiple additions at a time.
But the main advantage is that it can deal with situations where the number of channels is arbitrary or determined at runtime.

Three channels of Pulse are mixed to one channel:

	Sum(Pulse([400, 501, 600], [0.5, 0.1])) * 0.1

Multi channel expansion works differently for Mix.
Mix takes one input which is an array (one not protected by a Ref).
That array does not cause copies of Mix to be made.
All elements of the array are mixed together in a single Mix object.
On the other hand if the array contains one or more arrays then multi channel expansion is performed one level down.
This allows you to mix an array of stereo (two element) arrays resulting in one two channel array.
For example:

Currently it is not recursive. You cannot use Mix on arrays of arrays of arrays.

Here is a final example illustrating multi channel expansion and Mix.
By changing the variable _n_ you can change the number of voices in the patch.
How many voices can your machine handle?

	let n = 16 * 1; {- number of 'voices' -}
	Sum(
		EqPan( {- pan the voice -}
			CombL( {- a comb filter used as a string resonator -}
				Dust( {- random impulses as an excitation function -}
					{ Rand(0.9, 1.1) } ! n {- array expands Dust to n channels, approx. one impulse per second -}
				) * 0.3, {- amplitude -}
				0.01, {- max delay time in seconds -}
				{ Rand(0.0003, 0.004) } ! n, {- array of different random lengths for each 'string' -}
				4 {- decay time in seconds -}
			),
			{ 1.Rand2 } ! n {- give each voice a different pan position -}
		)
	)
