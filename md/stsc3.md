# stsc3

Command line stsc3.

## sc cat

Parse and pretty print SuperCollider program files.

The parser is for a minimal subset of SuperCollider
and does not recognise the following constructs:

~~~~
P()       => P.new()
p.(q)     => p.value(q)
`p        => Ref.new(p)
p q: r    => p.q(r)
q(p)      => p.q
|p|       => arg p;
p[q]      => p.at(q)
p[q]=r    => p.put(q,r)
p.q(*r)   => p.performList(\q,r)
p.q {}    => p.q({})
(p..q)    => p.to(q).asArray
(p,q..r)  => p.series(q,r)
~~~~

## st cat

Parse and pretty print Smalltalk program files.
There are two parsers,
one using `parsec` (better error messages)
and one using `alex` & `happy` (better grammar analysis and faster parsing).
They should be otherwise be equivalent.

~~~~
$ stsc3 st cat parsec ~/sw/hsc3-graphs/db/*.st
...
/home/rohan/sw/hsc3-graphs/db/09041bfdf77d32d2.st
|n x t f|
 n := Dseq repeats: #dinf list: #(1 3 2 7 8 32 16 18 12 24) mce .
 x := MouseX minval: 1 maxval: 10000 warp: 1 lag: 0.1 .
 t := Impulse freq: x phase: 0 .
 f := Demand trig: t reset: 0 demandUGens: n mul: 30 add: 340 .
 SinOsc freq: f phase: 0 mul: 0.1 .
...
$ stsc3 st cat happy ~/sw/hsc3-graphs/db/*.st
...
/home/rohan/sw/hsc3-graphs/db/fe1b361244cc392c.st
|f1 f2 f3 a3|
 f1 := 300 .
 f2 := 300 * 3 / 2 .
 f3 := (f2 - f1) abs .
 a3 := (SinOsc freq: 0.05 phase: 0 mul: 0.1) max: 0 .
 (SinOsc freq: {f1 . f2 . f3} mce phase: 0 mul: {0.1 . 0.1 . a3} mce) mix .
...
$
~~~~

## repl

There are two interpreters.

One is a simple implementation of the Simple Object Model.
It reads the location of class library files from the environment variable `SOM_CLASS_PATH`.
~~~~
$ stsc3 repl som
> TestHarness new run: #('TestHarness.som')
...
Total number of tests:           122
Number of unsupported optionals: 3
Number of successful tests:      122
Number of assertions tested:     640
> Harness new run: #('Harness' 'Bounce')
...
Total Runtime: 1924501us
^D
$
~~~~

The other is an interpreter for Smalltalk programs where the only
data type is the SuperCollider Unit Generator.  It has two forms, one
interprets the Ansi Ast directly, the other interprets a simplified
expression Ast.

This interpreter can run the Smalltalk SuperCollider help graphs
directly, without requiring a Smalltalk system.  However it is _not_ a
Smalltalk system, it is rather a dialect of Lisp with Smalltalk
syntax.  It is implemented using Haskell SuperCollider, and inherits
all of it's behaviour from that system.  Math at constant UGens is
optimised, so printing for constant expressions is ordinary.

~~~~
$ stsc3 repl ansi
1 + 2
result: 3.0
[:x | x * x] value: 4
result: 16.0
#(1 2 3) * 2
result: {2.0. 4.0. 6.0}
4 arrayFill: [:i | 5.0 rand]
result: {0.13. 2.61. 2.70. 1.44}
4 arrayFill: [:i | #(1 2 3 4 5) atRandom]
result: {4.0. 3.0. 2.0. 3.0}
#(#(1 2) #(3 4)) transpose concatenation
result: {1.0. 3.0. 2.0. 4.0}
^d
$
~~~~

## translate

There are two related but distinct translators.

The first translates a subset of SuperCollider to Smalltalk.

It follows the SuperCollider rules for message names and parameters (ie. parameters are always dictionaries).

The second translates a SuperCollider like notation called C-Smalltalk to Smalltalk.

C-Smalltalk is an alternate notation for writing Smalltalk programs.

C-Smalltalk files have a `.stc` file extension.

There are numerous examples at `stsc3/help/graph`, and they are also included in the `hsc3-graphs` polyglot archive.

There is an `stc.el` Emacs mode at `stsc3/emacs` which runs the translator and sends the translated text to GNU Smalltalk.

The translator reads the source text from `stdin` and writes the the translated text to `stdout`.

````
$ stsc3 -h | grep translate
 translate {sc | stc} st
$
````
