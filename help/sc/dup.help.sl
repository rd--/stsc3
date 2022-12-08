# dup - duplicate

- dup(anObject, anInteger): answer an _Array_ constructed by applying _value_ to _anObject_ _anInteger_ times

Ordinarily _anObject_ is a no-argument _Procedure_ and each element of the answer is a result of evaluating the procedure.

The operator _!_ is an alias for _dup_.

```
{
	SinOsc({ IRand(48, 72).MidiCps } ! 7, 0).Splay2 * 0.1
}.overlap(4, 4, 2)
```
