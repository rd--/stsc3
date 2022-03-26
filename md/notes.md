# Remainder

- build graphs in smalltalk
- .stc to .st translator writing SinOsc(f, p) as (SinOsc freq: f phase: p)
- .stc to .sc translator writing f(x) as f.apply(x)
- program to collate help graphs into methods, ie. ScHelpGraph class>>jmccAlienMeadow

# Form

The current bindings delay _shape_ operations.
_SinOsc([440, 441], 0)_ makes a _SinOsc_ object, not an _Array_ object.
Shape operations, such as _reverse_, must be part of the same family as _Resonz_, that is they must be named.
Operations that need to traverse the shape such as _inject_,say to find the per-frame maximum of an array of signals, need to specify their operator as a name also.
The rationale for this model was that the object structure reflects the written structure, and that the object structure could be edited.
_blksc3_ is a simpler mechansim for implementing this idea.
