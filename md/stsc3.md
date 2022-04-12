# stsc3

Command line stsc3.

## sc cat fragment

Parse and pretty print SuperCollider program files.

The parser is for a minimal subset of SuperCollider
and does not recognise the following constructs:

~~~~
p.(q)     => p.value(q)
`p        => Ref.new(p)
p q: r    => p.q(r)
|p|       => arg p;
p[q]      => p.at(q)
p[q]=r    => p.put(q,r)
p.q(*r)   => p.performList(\q,r)
p.q {}    => p.q({})
(p..q)    => p.series(nil,q)
(p,q..r)  => p.series(q,r)
~~~~

## sc cat library

~~~~
$ stsc3 sc cat library ~/sw/stsc3/help/expr/library.sc
/home/rohan/sw/stsc3/help/expr/library.sc
...
H A { classvar y, z; var x, w;  m { arg n; ^x * n } }
I A { classvar y, z; var x, w; *c { arg d; ^z + d } m { arg n; ^x * n } }
...
$
~~~~

## sc cat extensions

~~~~
$ stsc3 sc cat extensions ~/sw/stsc3/help/expr/extensions.sc
/home/rohan/sw/stsc3/help/expr/extensions.sc
+A {  f { arg a; ^a * v } }
+B {  * { arg aNumber; ^this.mul(aNumber) } }
...
$
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

## translate

There are two related translators.

The first translates a subset of the SuperCollider syntax as a C-like notation for Smalltalk programs.

There are examples at `stsc3/help/graph`, and these are included in the `hsc3-graphs` polyglot archive.

The files have a`.stc` file extension (for C-Smalltalk).

There is an `stc.el` Emacs mode at `stsc3/emacs` which runs the translator and sends the translated text to GNU Smalltalk.

The second translator additionally rewrites message parameters as arrays with optional keywords (a kind of dictionary)
and can be used to translate SuperCollider programs to Smalltalk programs.

In both cases the translator reads the source text from `stdin` and writes the the translated text to `stdout`, unless named files are given.

````
$ stsc3 -h | grep translate
 translate {stc | sc} st [input-file output-file]
$ stsc3 translate stc st < help/graph/jmcc-analog-bubbles.stc
|o f|
o := (LFSaw apply: {#(8 7.23) . 0}) * 3 + 80 .
f := (LFSaw apply: {0.4 . 0}) * 24 + o .
(CombN apply: {(SinOsc apply: {f midicps . 0}) * 0.04 . 0.2 . 0.2 . 4}) * 0.1 .
$
````
