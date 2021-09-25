# C-Smalltalk

A C-like syntax for [Smalltalk](https://squeak.org/).

The syntax is essentially the core [SuperCollider](https://www.audiosynth.com/) syntax, which is C-like.

C-Smalltalk syntax is an interesting complement to Smalltalk syntax.

It supports:

- a concise notation for performing a _primary_ message at an object
- a concise and uniform notation for chaining both unary and n-ary messages
- a two-level precedence model (unary and n-ary messages bind equally and more closely than binary messages)
- implicit grouping of message argument expressions
- a notation for binding variables when they are declared

## Relation to SuperCollider Syntax

C-Smalltalk utilises a subset of the SuperCollider syntax as an alternate notation for writing Smalltalk programs.

C-Smalltalk programs will not ordinarily be valid SuperCollider programs without extending the SuperCollider class library. [1]

In addition C-Smalltalk has one syntactic extension (colons within message names) that is incompatible with SuperCollider.

## Implicit primary message notation

The notation `SinOsc(440, 0)` is allowed in SuperCollider but has a different meaning in C-Smalltalk.

It is syntax for `SinOsc.apply([440, 0])`.

`apply:` is ordinarily defined as `perform:withArguments:` at the `primaryFactoryMethod` of the receiver. [2]
(This terminology is from [Newspeak](https://newspeaklanguage.org/).)

The primaryFactoryMethod of `SinOsc` is `freq:phase:`, and so on for all of the SuperCollider UGens.

UGens that have no parameters, such as PinkNoise, have `new` as their primaryFactoryMethod, and can be instantiated as `PinkNoise()`.

This translation is uniform for all primary identifiers. [3]

The rationale for this notation is that certain identifiers (class names in particular) often imply a primaryFactoryMethod,
and that in such cases it can be appropriate to elide the message name.

## N-ary methods and implicit keyword message names

The C-Smalltalk syntax allows message names to have interior `:` characters.

`c.at:put(i, j)` translates to `c at: i put: j`.

In addition, if the number of parts of the message name is less than the number of arguments to the message, the message name is extended with implicit `value` parts.

`f.value(i, j)` translates as `f value: i value: j` and `c.put(i, j)` translates as `c put: i value: j`.

It is therefore possible to provide aliases for commonly used messages, i.e. `put:value:` as an alias for `at:put:`.

It is an error for the message name to have more parts than there are arguments, i.e. `r.p:q(i)` is an error. [4]

## Keywords

Keyword parameters are disallowed.

They could be allowed for implicit message sends and checked against the primaryFactoryMethod.

In cases where keyword arguments are appropriate, ordinary Smalltalk syntax is preferred.

* * *

1: In addition to the C-Smalltalk to Smalltalk translator `stsc3` also has a SuperCollider to Smalltalk translator.

C-Smalltalk is two of the three rewriting rules of the the SuperCollider to Smalltalk translator,
see [Rewrite.hs](https://gitlab.com/rd--/stsc3/-/blob/master/Language/Smalltalk/SuperCollider/Rewrite.hs) for details.

2: apply: is generally defined as:

````
apply: arg
    ^self perform: (self primaryFactoryMethod) withArguments: arg
````

3: This is in distinction to SuperCollider where
   `X(i, j, k)` translates as `X.new(i, j, k)` and
   `x(i, j, k)` translates as `i.x(j, k)`.

````
Rand(0,9) // a Rand
{var r = Rand; r(0,9)}.value // error: r not understood by 0
````

It is not as common for non `Class` objects to understand `primaryFactoryMethod`, however `arg c; c(...);` is a standard idiom.

4: Implicit keyword message names is not a form of variable arity messages.
`r.m(i)` and `r.m(i, j)` are distinct messages, `m:` and `m:value:`.
