# C-Smalltalk

C-Smalltalk is  C-like syntax for [Smalltalk](https://squeak.org/).
C-Smalltalk is, for the most part, a subset of the [SuperCollider](https://www.audiosynth.com/) syntax.

C-Smalltalk syntax is an interesting complement to Smalltalk syntax.

It supports:

- a concise notation for performing a primary message at an object
- a concise and uniform notation for chaining both unary and n-ary messages
- a two-level precedence model (unary and n-ary messages bind equally and more closely than binary messages)
- implicit grouping of message argument expressions
- a notation for binding variables when they are declared

C-Smalltalk files have the extension _.stc_.

The _stc_ Emacs mode includes keybindings to translate C-Smalltalk expressions to Smalltalk,
and then forward the translation to a Smalltalk interpreter.

## Relation to SuperCollider Syntax

C-Smalltalk utilises a subset of the SuperCollider syntax as an alternate notation for writing Smalltalk programs.

C-Smalltalk programs will not ordinarily be valid SuperCollider programs without extending the SuperCollider class library. [1]

In addition C-Smalltalk has one syntactic extension (colons within message names) that is incompatible with SuperCollider.

## Implicit primary message notation

The notation _SinOsc(440, 0)_ is allowed in SuperCollider, where it has the meaning _SinOsc.new(440, 0)_.

In C-Smalltalk it has a related but distinct meaning, _SinOsc.apply([440, 0])_.

_apply:_ is ordinarily defined as _perform:withArguments:_ at the _primaryFactoryMethod_ of the receiver. [2]
(This terminology is from [Newspeak](https://newspeaklanguage.org/).)

The primaryFactoryMethod of _SinOsc_ is _freq:phase:_, and so on for all of the SuperCollider UGens.

UGens that have no parameters, such as PinkNoise, have _new_ as their primaryFactoryMethod, and can be instantiated as _PinkNoise()_.

This translation is uniform for all primary identifiers. [3]

The rationale for this notation is that certain identifiers (class names in particular) often imply a primaryFactoryMethod,
and that in such cases it can be appropriate to elide the message name.

## N-ary methods and implicit keyword message names

The C-Smalltalk syntax allows message names to have interior colon (_:_) characters.

_c.at:put(i, j)_ translates to _c at: i put: j_.

In addition, if the number of parts of the message name is less than the number of arguments to the message, the message name is extended with implicit _value_ parts.

_f.value(i, j)_ translates as _f value: i value: j_ and _c.put(i, j)_ translates as _c put: i value: j_.

It is therefore possible to provide aliases for commonly used messages, i.e. _put:value:_ as an alias for _at:put:_.

It is an error for the message name to have more parts than there are arguments, i.e. _r.p:q(i)_ is an error. [4]

## Interspersed Unary and Keyword message sequences

C-Smalltalk requires parentheses for all n-ary messages, and unary and n-ary messages have equal precedence.

This can result in a perspicuous notation for chains of message sends.

_c.reverse.at(n).sorted.collect(f).display_ translates to
_((c reverse at: n) sorted collect: f) display_.

## Keywords

Keyword parameters are disallowed.

They could be allowed for implicit message sends and checked against the primaryFactoryMethod.

However in cases where keyword arguments are appropriate, ordinary Smalltalk syntax is preferred.

## Translation

C-Smalltalk can be used as a notation for _Scheme-like_ languages.
Unary messages _p.q_ are translated as _q(p)_, operators _p + q_ as _+(p, q)_, and n-ary messages _p.q(r, s)_ as _q(p, r, s)_.
_stsc3_ includes a [translator](https://rohandrape.net/pub/stsc3/html/stc-to-js.html) from C-Smalltalk to Javascript.

## Unary and binary message selectors

In C-Smalltalk _p.q_ and _p.q(r)_ refer to two distinct selectors, _q_ in the first case and _q:_ in the second.
When C-Smalltalk is translated into a _scheme-like_ notation and semantics this distinction is not made.
The first case translates as _q(p)_ and the second as _q(p, r)_.
This requires _q_ to have two arities, and while many scheme-like languages will allow this, it is an unnecessary complication.
For this reason, if translation is desired, unary and binary message selectors should be named so that they translate distinctly.

* * *

1: In addition to the C-Smalltalk to Smalltalk translator _stsc3_ also has a SuperCollider to Smalltalk translator.

This second translator additionally rewrites message parameters as arrays with optional keywords (a kind of dictionary).
See [Rewrite.hs](https://gitlab.com/rd--/stsc3/-/blob/master/Language/Smalltalk/SuperCollider/Rewrite.hs) for details.

2: apply: is generally defined as:

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

It is not as common for non Class_ objects to understand _primaryFactoryMethod_,
however _arg c; c(...);_ is a common idiom where c is a class object.

4: Implicit keyword message names are not a form of variable arity messages.
_r.m(i)_ and _r.m(i, j)_ are distinct messages, _m:_ and _m:value:_.
