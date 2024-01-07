# Unit Generators and Synths

A unit generator is an object that processes or generates sound. There are many classes of unit generators, all of which derive from the class UGen.

Unit generators in SuperCollider can have many inputs, but always have a single output. Unit generator classes which would naturally have several outputs such as a panner, return an array of unit generators when instantiated.  The convention of having only a single output per unit generator allows the implementation of multiple channels by using arrays of unit generators.

## Instantiation. Audio Rate, Control Rate

A unit generator is created by a sending the 'ar' or 'kr' message to the class object of the unit generator. The 'ar' message creates a unit generator that runs at audio rate. The 'kr' message creates a unit generator that runs at control rate. Control rate unit generators are used for low frequency or slowly changing control signals. Control rate unit generators produce only a single sample per buffer and therefore use less processing power than audio rate unit generators.

The input parameters for a unit generator are given in the documentation for that class.

	SinOsc(800, 0) * 0.1 (* create a sine oscillator at 800 Hz, amplitude 0.1 *)

A signal inputs of a unit generator can be other unit generators, scalars, or arrays of unit generators and scalars.

## Synth objects

In order to play a unit generator one needs to install it in a Synth object. A Synth is a container for one or more unit generators that execute as a group. The Synth : new method evaluates a function passed in as an argument to create a unit generator network.

The 'play' method of class Function will create and play a synth using the function for you:

	FSinOsc(800, 0).Mul(0.1).play

## Building Patches

You can do math operations on unit generators and the result will be another unit generator. Doing math on unit generators is not doing any signal calculation itself - it is building the network of unit generators that will execute once they are played in a Synth. This is the essential thing to understand: Synthesis networks, or in other words signal flow graphs are created by executing expressions of unit generators.  The following expression creates a flow graph whose root is an instance of BinaryOpUGen which performs the '+' operation. Its inputs are the FSinOsc and BrownNoise unit generators.

	(SinOsc(800, 0) * 0.1) + (BrownNoise() * 0.1) (* use cmd-P *)

You can find out what unit generators are in a Synth by dumping the Array stored in the 'ugens' instance variable. The unit generators are listed in the order they will be executed.
