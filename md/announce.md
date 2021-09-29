Smalltalk SuperCollider

Smalltalk SuperCollider is a Smalltalk library for working with the SuperCollider synthesiser.

Smalltalk SuperCollider is research quality, it has minimal documentation and the current implementation is in terms of Haskell SuperCollider, which is required in addition to a Smalltalk image.

It does work though, and it may be of interest to people familiar with SuperCollider, Smalltalk and Haskell.

Smalltalk SuperCollider is at <http://rd.slavepianos.org/t/stsc3>

Haskell SuperCollider is at <http://rd.slavepianos.org/t/hsc3>

There are some short demonstration videos at <http://rd.slavepianos.org/?t=stsc3&e=md/video.md>

Smalltalk SuperCollider also includes an interpreter for Smalltalk programs where the only data type is the SuperCollider Unit Generator.
This interpreter (called "stsc3") can run the example graphs in the "help" sub-directories directly, without requiring a Smalltalk system.

Smalltalk SuperCollider works with Squeak, GNU Smalltalk, Pharo and Cuis.
There is an Emacs mode for working with either GNU Smalltalk or the stsc3 interpreter.

It should be relatively simple to add a mechanism for communicating directly with SuperCollider, patches welcome.

Best,
Rohan

scsynth/[3897](https://scsynth.org/t/smalltalk-supercollider/3897) ; 2021-06-14

* * *

<https://scsynth.org/t/supercollider-smalltalk/4602>

SuperCollider Smalltalk

stsc3 now has translators from SuperCollider to Smalltalk.

There are two forms, the nicer one allows writing Smalltalk programs using SuperCollider syntax.

It works particularly well for Smalltalk classes that have a _primary factory message_.

There are example files at `stsc3/help/graphs` (they have an `.stc` extension).

One is copied here for reference:

````
// zizle (jmcc) #SC3d1.5 ; texture=overlap,4,4,12,inf
var a = {arg f; (SinOsc(f * [Rand(0.7, 1.3), 1], {Rand(0, 2 * pi)}.dup) * 0.1).mix};
var o = SinOsc(Rand(24, 108).midicps, Rand(0, 2 * pi));
var s = o * a.value(ExpRand(0.3, 8)).max(0) * a.value(ExpRand(6, 24)).abs;
Pan2(s, Rand(-1, 1), 1)
````

The Squeak SC3 Browser translates as required, and there's an Emacs mode work with GNU Smalltalk.

stsc3 is at <https://gitlab.com/rd--/stsc3>

Best,
Rohan

Ps. Since this is not straightforwards to get working, here's a short video of a lovely Fredrik Olofsson graph, for the mildly curious.
It indicates a subtle difference in the rules for implicit message names, c.f. `...osc = LFTri; osc(1 / b, 0)...`.  <https://vimeo.com/615732677>

scsynth/[3897](https://scsynth.org/t/supercollider-smalltalk/4602>) ; 2021-09-27
