# LfPulse vs. Pulse and LfSaw vs. Saw

In different situations you will want to use different versions of these unit generators.
The _Lf_ versions are faster but are not band limited.
That means that they can fold over the Nyquist frequency and cause aliased frequencies.
For example here is the same frequency sweep using both kinds of oscillators:

`LfPulse` will alias at high frequencies.

~~~spl
LfPulse(XLine(1000, 8000, 5), 0, 0.1) * 0.2
~~~

`Pulse` is band limited and therefore smoother.

~~~spl
Pulse(XLine(1000, 8000, 5), 0.1) * 0.2
~~~

Another pair of examples to compare, `Pulse`:

~~~spl
Pulse(SinOsc(0.75, 0) * 5800 + 6000, 0.1) * 0.2
~~~

and `LfPulse`:

~~~spl
LfPulse(SinOsc(0.75, 0) * 5800 + 6000, 0, 0.1) * 0.2
~~~

`LfPulse` is better to use as a low frequency controller because it is truly a rectangular wave,
plot the below to see the difference,
note also the opposite values for width:

~~~spl
[LfPulse(100, 0, 0.3), Pulse(100, 0.7)] * 0.1
~~~

The same issues apply to `Saw` and `LfSaw`.

`LfSaw` will alias at high frequencies.

~~~spl
LfSaw(XLine(1000, 8000, 5), 0) * 0.2
~~~

`Saw` is band limited and therefore smoother.

~~~spl
Saw(XLine(1000, 8000, 5)) * 0.2
~~~

Another pair of examples to compare, `Saw`:

~~~spl
Saw(SinOsc(0.75, 0) * 5800 + 6000) * 0.2
~~~

and `LfSaw`:

~~~spl
LfSaw(SinOsc(0.75, 0) * 5800 + 6000, 0) * 0.2
~~~

`LfSaw` is better to use as a low frequency controller because it is truly a sawtooth shape,
plot the below to see the difference,
note also phase offset for `LfSaw`,
and that it is ascending where `Saw` is descending,
and amplitude difference:

~~~spl
[LfSaw(100, 1), Saw(100)] * 0.1
~~~
