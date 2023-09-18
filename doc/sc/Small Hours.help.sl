# Small Hours

Small Hours is an experimental interpreter for the experimental [_Simple Programming Language_].

It includes a library for communicating with the _SuperCollider_ synthesiser.

To play a synthesiser program select it and type _Ctrl-Enter_.

```
(* https://sccode.org/1-4Qy ; f0 ; 0283 *)
var b = 1 / (2 .. 6);
var o1 = SinOscFb(
	SinOscFb(b, 1) < b * 500 + 99,
	0
) / 5;
var o2 = SinOscFb(
	999 * b,
	SinOscFb(SinOscFb(b, 1) < 0.1 + 1, 1) % b
);
var o3 = SinOscFb(
	0.1 - b,
	1
).Min(0);
Splay2(o1 + (o2 * o3)) / 2
```

To reset the synthesiser type _Ctrl-FullStop_.

To get help about a word, for instance _SinOscFb_, select it and type _Ctrl-QuestionMark_.

Text in brackets, for instance _[Simple Editor]_, indicates that the bracketed text is the name of a help document,
which can be visited the same way.

There is a language [Help Index] and an [Sc Help Index] where terms are sorted alphabetically.
