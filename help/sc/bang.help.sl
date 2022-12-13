# ! ≡bang - duplicate

- _anObject ! anInteger_

Answer an _Array_ constructed by applying _value_ to _anObject_ _anInteger_ times

Ordinarily _anObject_ is a no-argument _Procedure_ and each element of the answer is a result of evaluating the procedure.

```
{
	SinOsc({ IRand(48, 72).MidiCps } ! 7, 0).Splay2 * 0.1
}.overlap(4, 4, 2)
```

This can be more written more simply using the _!^_ operator as:

```
{
	{ SinOsc(IRand(48, 72).MidiCps, 0) } !^ 7 * 0.1
}.overlap(4, 4, 2)
```

* * *

See also: _dup_, _replicate_
