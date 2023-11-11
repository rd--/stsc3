# 2. How to use Ugens

## 2.1 What is a Ugen?

A unit generator is an object that processes or generates sound.  There are many classes of unit generators, all of which derive from the class Ugen.

All of the Ugen classes are listed in the help file [Ugen_Ref_Sheet].  From there you can get help on any unit generator by double clicking on the class name and doing cmd-H.  Each Ugen class help file has executable examples of its use.

## 2.2 How to create a Ugen

A unit generator is created by sending the 'ar' or 'kr' message to the class object of the unit generator. The 'ar' message creates a unit generator that runs at audio rate. The 'kr' message creates a unit generator that runs at control rate. Control rate unit generators are used for low frequency or slowly changing control signals. Control rate unit generators produce only a single sample per block and therefore use less processing power than audio rate unit generators. Since block sizes are typically 64 samples, a control rate unit generator could theoretically be 1/64 the cost, however the savings is not quite that great due to setup overhead. The savings is significant though, so anytime a control rate ugen will serve, it should be used.

Create an audio rate sine oscillator:

	SinOsc(800, 0) * 0.1

Create a control rate sine oscillator:

	SinOsc(800, 0) * SinOsc(2, 0).kr * 0.1

The input parameters for a unit generator are given in the documentation for that class.

In order to create sound, a Ugen must be created inside of a Synth ugenGraphFunction.

	{ SinOsc(800, 0) * 0.1 }.play

## 2.3 Doing math with Ugens

You can do math operations on unit generators and the result will be another unit generator. Doing math on unit generators is not doing any signal calculation itself - it is building the network of unit generators that will execute once they are played in a Synth. This is the essential thing to understand: Synthesis networks, or in other words signal flow graphs are created by executing expressions of unit generators.  The following expression creates a flow graph whose root is an instance of BinaryOpUgen which performs the '+' operation. Its inputs are the FSinOsc and BrownNoise unit generators. FSinOsc is a fast fixed frequency sine oscillator and BrownNoise is a noise generator that emphasizes lower frequencies.

Add signals, use play command:

	(FSinOsc(800, 0) * 0.2) + (BrownNoise() * 0.2)

With _{ }.play_ written in, use evaluate command:

	{ (FSinOsc(800, 0) * 0.2) + (BrownNoise() * 0.2) }.play

Multiply signals:

	(FSinOsc(800, 0) * 0.4) * (BrownNoise() * 0.5)

## 2.4 Signal level

The output signal level of most unit generators that generate audio is from -1 to +1.  There are a few exceptions like COsc which ranges from -2 to +2, or _LfPulse_ which ranges from 0 to +1. You can use the Synth plot method to check the range of any Ugen.

A 200 Hz sine wave:

	{ FSinOsc(200, 0) * 0.1 }.plot

White noise:

	{ WhiteNoise() * 0.1 }.plot

Brown noise:

	{ BrownNoise() * 0.1 }.plot

Pink noise is capable of -1 to +1 but is statistically unlikely to acheive it

	{ PinkNoise() * 0.1 }.plot

500 Hz pulse wave with a 30% duty cycle:

	{ LfPulse(500, 0, 0.3) * 0.1 }.plot

The output signal level of Ugens that process their input such as filters and delays depend on the level of the input and the settings of the Ugen's particular controls.

## 2.5 Mul and Add inputs

Many unit generators have inputs named mul and add which allow you to multiply and add signals to that Ugen's output. Using mul and add is more efficient that using an explicit * or + operator.  Also it is often desirable to scale and offset the values of control Ugens to match some specific range for purposes of modulation.  The values for mul and add default to 1 and 0 respectively, which leaves the output unchanged.  With these defaults, the multiply and add are optimized out and there is no computational cost for them.

	{ FSinOsc(200) },plot (* a 200 Hz sine wave *)
	{ FSinOsc(200) * 0.2 },plot (* 200 Hz sine wave with mul=0.2 *)
	{ FSinOsc(200) * 0.2 + 0.2 },plot (* 200 Hz sine wave with mul=0.2 and add=0.2 *)
	{ FSinOsc(200) * 0.5 + 0.5 },plot (* 200 Hz sine wave with mul=0.5 and add=0.5 *)
	{ FSinOsc(200) * 0.5 + -0.5 },plot (* 200 Hz sine wave with mul=0.5 and add=-0.5 *)

Add can be used to mix signals.  This example is equivalent to one given in 2.3 above.  The noise generator is plugged into the add input of FSinOsc

	FSinOsc(800, 0) + BrownNoise() * 0.1

Mul can be used to ring modulate signals.  This example is equivalent to another one given in 2.3 above.  The noise generator is plugged into the mul input of FSinOsc

	FSinOsc(800, 0) * (BrownNoise() * 0.2)

You can plug control rate Ugens into the mul and add inputs of an audio rate Ugen.  While plugging an audio rate Ugen into the mul or add input of a control rate Ugen is possible, it is wasteful of CPU since a control rate Ugen will sub-sample any audio rate input.

## 2.6 Modulation

The signal inputs of a unit generator can be other unit generators, scalars, or Arrays of unit generators and scalars. Using Arrays as inputs will be covered in the section on multiple channels.  Here we show some examples of using unit generators as inputs to other unit generators.

This example modulates the frequency of a sine oscillator with a linear function, a simple frequency sweep:

	SinOsc( (* create a sine oscillator *)
		Line( (* modulate the frequency with a Line unit generator *)
			2000, (* the line begins at 2000 *)
			300, (* and ends at 300 *)
			10 (* in 10 seconds *)
		),
		0 (* zero phase *)
	) * 0.1 (* amplitude 0.1 *)

The short version of the above:

	SinOsc(Line(2000, 300, 10), 0) * 0.1

In the following example the frequency modulator is changed from a line to a sine oscillator.

	SinOsc( (* create a sine oscillator *)
		SinOsc( (* modulate the frequency with another sine oscillator *)
			0.5, (* the modulating sine freq is 1 cycle per 2 seconds *)
			0 (* zero phase *)
		) * 300 (* mul = 300 *)
		+ 700, (* add = 700 *)
		(* These settings of mul and add will cause the
		frequency to vary from -300+700 to +300+700 Hz
		which equals from 400 to 1000 Hz *)
		0) (* zero phase *)
	* 0.1 (* amplitude 0.1 *)

Now we will modulate the modulator to cause the frequency modulation to speed up over time, i.e. sweep the frequency of an _Lfo_

	SinOsc( (* create a sine oscillator *)
		SinOsc(
			(* modulate the frequency with another sine oscillator
			make the freq modulator speed up exponentially
			by modulating its frequency with another Ugen *)
			XLine( (* make an exponential line generator *)
				0.5, (* begin at 0.5 Hz *)
				100, (* end at 100 Hz *)
				30 (* in 30 seconds *)
			),
			0) (* zero phase *)
		* 300 (* mul = 300 *)
		+ 700, (* add = 700 *)
		(* These settings of mul and add will cause the
		frequency to vary from -300+700 to +300+700 Hz
		which equals from 400 to 1000 Hz *)
		0) (* zero phase *)
	* 0.1 (* amplitude 0.1 *)

The nested style used above can become hard to read, so often it is preferable to use variables to make it more readable.

	var lfofreq = XLine( (* make an exponential line generator *)
		0.5, (* begin at 0.5 Hz *)
		100, (* end at 100 Hz *)
		30 (* in 30 seconds *)
	);
	var freq = SinOsc( (* modulate the frequency with another sine oscillator *)
		lfofreq, (* use exponential line to modulate the lfo freq *)
		0) (* zero phase *)
	* 300 (* mul = 300 *)
	+ 700; (* add = 700 *)
	SinOsc( (* create a sine oscillator *)
		freq, (* frequency modulator *)
		0) (* zero phase *)
	* 0.1 (* amplitude 0.1 *)
