# C-Smalltalk

C-Smalltalk is  C-like notation for [Smalltalk](https://squeak.org/).
C-Smalltalk is, for the most part, a subset of the [SuperCollider](https://www.audiosynth.com/) notation.

C-Smalltalk notation is an interesting complement to Smalltalk notation.

It supports:

- a concise notation for performing a primary message at an object
- a concise and uniform notation for chaining both unary and n-ary messages
- a two-level precedence model (unary and n-ary messages bind equally and more closely than binary messages)
- implicit grouping of message argument expressions
- a notation for binding variables when they are declared
- a notation for writing class definitions and for extending existing classes

C-Smalltalk files have the extension _.stc_.

The _stc_ Emacs mode includes keybindings to translate C-Smalltalk expressions to Smalltalk,
and then forward the translation to a Smalltalk interpreter.

## Relation to SuperCollider

C-Smalltalk utilises a subset of the SuperCollider syntax as an alternate notation for writing Smalltalk programs.

C-Smalltalk programs will not ordinarily be valid SuperCollider programs without extending the SuperCollider class library.

## Implicit primary message notation

The notation _SinOsc(440, 0)_ is allowed in SuperCollider, where it has the meaning _SinOsc.new(440, 0)_.

In C-Smalltalk it has a related but distinct meaning, _SinOsc.apply([440, 0])_.

_apply:_ is ordinarily defined as _perform:withArguments:_ at the _primaryFactoryMethod_ of the receiver. [1]
(This terminology is from [Newspeak](https://newspeaklanguage.org/).)

The primaryFactoryMethod of _SinOsc_ is _freq:phase:_, and so on for all of the SuperCollider UGens.

UGens that have no parameters, such as PinkNoise, have _new_ as their primaryFactoryMethod, and can be instantiated as _PinkNoise()_.

This translation is uniform for all primary identifiers. [2]

The rationale for this notation is that certain identifiers (class names in particular) often imply a primaryFactoryMethod,
and that in such cases it can be appropriate to elide the message name.

If _BlockClosure_ has an instance definition of _apply:_ defined as _valueWithArguments:_
then the notation _f(p, q)_ where _f_ is a block has the meaning _f value: p value: q_.
This is not, however, compatible with SuperCollider notation, where _f(p, q)_ has the meaning _p.f(q)_.

## Implicit keyword message names

If the number of parts of the message name is less than the number of arguments to the message, the message name is extended with implicit _value_ parts.

_f.value(i, j)_ translates as _f value: i value: j_ and _c.put(i, j)_ translates as _c put: i value: j_.

It is therefore possible to provide aliases for commonly used messages, i.e. _put:value:_ as an alias for _at:put:_.

## Keyword parameter notation

The C-Smalltalk notation _collection.at(key, put: value)_ means _collection at: key put: value_.
The message selector is _at:key:_.
The first parameter _must not_ have a keyword and _all_ following parameters _must_ have a keyword.

The C-Smalltalk notation _SinOsc(freq: 440, phase: 0)_ has the meaning _SinOsc.freq(440, phase: 0)_,
which has the meaning _SinOsc freq: 440 phase: 0_.

Likewise _collection(at: key, put: value)_ means _collection.at(key, put: value)_.

## Optional Parameters and Default Values

In SuperCollider notation keyword arguments are a mechanism for sending a message as a _dictionary_,
parameters may be provided out of order,
and parameters that are elided receive _default_ values.

These are disallowed in C-Smalltalk.

## Interspersed Unary and N-ary message sequences

C-Smalltalk requires parentheses for all n-ary messages, and unary and n-ary messages have equal precedence.

This can result in a perspicuous notation for chains of message sends.

_c.reverse.at(n).sorted.collect(f).display_ translates to
_((c reverse at: n) sorted collect: f) display_.

## Translation

C-Smalltalk can be used as a notation for _Scheme-like_ languages.

Unary messages _p.q_ are translated as _q(p)_, operators _p + q_ as _+(p, q)_, and n-ary messages _p.q(r, s)_ as _q(p, r, s)_.

Note that the final _:_ is elided, _p.q(r)_ is not translated as _q:(p, r)_.

Note also that trailing _value:_ elements of selectors are elided, _p.q(r, s)_ is not translated as _q:value(p, r, s)_.

In addition there may be a rule to translate selectors, for instance from _at:put_ to _atPut_.

_stsc3_ includes a [translator](https://rohandrape.net/pub/stsc3/html/stc-to-js.html) from C-Smalltalk to Javascript.

## Unary and binary message selectors

In C-Smalltalk _p.q_ and _p.q(r)_ refer to two distinct selectors, _q_ in the first case and _q:_ in the second.
When C-Smalltalk is translated into a _scheme-like_ notation and semantics this distinction is not made.
The first case translates as _q(p)_ and the second as _q(p, r)_.
This requires _q_ to have two arities, and while many scheme-like languages will allow this, it is an unnecessary complication.
For this reason, if translation is desired, unary and binary message selectors should be named so that they translate distinctly.

## Notation for the _at:_ and _at:put:_ protocol

The C-like notations _p[q]_ and _p[q] = r_ translate as _p at: q_ and _p at: q put: r_ respectively.
This protocol is very widely implemented and a concise and familiar notation seems useful.
The _at:put:_ notation forms an _expression_, at the same syntax level as the _assignment_ syntax.

## Notation for _Array_ and _Dictionary_ expressions

In addition to the literal _Array_ notation _#(1 2 3)_ most Smalltalks allow the array notation _{p. q. r}_ meaning _Array with: p with: q with: r_.
C-Smalltalk writes this as _[p, q, r]_.

In addition C-Smalltalk has a notation for writing _Dictionary_ expressions,
_(a: 1, b: 2)_ means _Dictionary new add: #a -> 1 ; add: #b -> 2 ; yourself_,
or _Dictionary newFromPairs: {#a. 1. #b. 2}_.
This notation requires keys to be identifiers.

# Notation for class definitions

The notation _c : p { var v; m { ^nil } }_ defines a class _c_, which is a subclass of _p_, with an instance variable _v_ and an instance method _m_.

The notation _c { classvar v; *m { ^nil } }_ defines a class _c_, with an class variable _v_ and an class method _m_.

Class variables must precede instance variables and class methods must precede instance methods.

The notation _+ c { m { ^nil } }_ adds an instance method _m_ to the existing class _c_.

* * *

1: apply: is generally defined as:

````
apply: arg
    ^self perform: (self primaryFactoryMethod) withArguments: arg
````

3: This is in distinction to SuperCollider where
   _X(i, j, k)_ translates as _X.new(i, j, k)_ and
   _x(i, j, k)_ translates as _i.x(j, k)_.

````
Rand(0,9) // a Rand
{var r = Rand; r(0,9)}.value // error: r not understood by 0
````

It is not as common for non _Class_ objects to understand _primaryFactoryMethod_,
however _arg c; c(...);_ is a common idiom where c is a class object.

4: Implicit keyword message names are not a form of variable arity messages.
_r.m(i)_ and _r.m(i, j)_ are distinct messages, _m:_ and _m:value:_.
