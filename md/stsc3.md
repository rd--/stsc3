# stsc3

Command line stsc3.

## cat

Parse and pretty print Smalltalk program files.
There are two parsers,
one using `parsec` (better error messages)
and one using `alex` & `happy` (better grammar analysis and faster parsing).
They should be otherwise be equivalent.

~~~~
$ stsc3 cat parsec ~/sw/hsc3-graphs/db/*.st
...
/home/rohan/sw/hsc3-graphs/db/09041bfdf77d32d2.st
|n x t f|
 n := Dseq repeats: #dinf list: #(1 3 2 7 8 32 16 18 12 24) mce .
 x := MouseX minval: 1 maxval: 10000 warp: 1 lag: 0.1 .
 t := Impulse freq: x phase: 0 .
 f := Demand trig: t reset: 0 demandUGens: n mul: 30 add: 340 .
 SinOsc freq: f phase: 0 mul: 0.1 .
...
$ stsc3 cat happy ~/sw/hsc3-graphs/db/*.st
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

An interpreter for Smalltalk programs where the only data type is the
SuperCollider Unit Generator.  The interpreter can run the Smalltalk
SuperCollider help graphs directly, without requiring a Smalltalk
system.  However it is _not_ a Smalltalk system, it is rather a
dialect of Lisp with Smalltalk syntax.  It is implemented using
Haskell SuperCollider, and inherits all of it's behaviour from that
system.  Math at constant UGens is optimised, so printing for constant
expressions is ordinary.

~~~~
$ stsc3 repl
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
