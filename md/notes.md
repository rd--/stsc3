# Remainder

- Sc
  + allow self for this
  + dictionary expressions at Ast
- build graphs in smalltalk
- .stc to .st translator writing SinOsc(f, p) as (SinOsc freq: f phase: p)
- .stc to .sc translator writing f(x) as f.apply(x)
- program to collate help graphs into methods, ie. ScHelpGraph class>>jmccAlienMeadow  (see SC3-Help.st)
  + requires re-write as final statement must be returned
  + list of help graphs in list view with menu to play, draw, visit entries
- add 'play it!' and 'reset supercollider' menu items to workspace menu

# Form

The current bindings delay _shape_ operations.

_SinOsc([440, 441], 0)_ makes a _SinOsc_ object, not an _Array_ object.

Shape operations, such as _reverse_, must be part of the same family as _Resonz_, that is they must be named.

Operations that need to traverse the shape such as _inject_, say to find the per-frame maximum of an array of signals, need to be named and also specify their operator as a name.

There are nice aspects to this model:

1. the object structure reflects the written structure, the _SinOsc_ constructor method actually makes a _SinOsc_ object
2. the object structure can be edited
3. the object struture can be printed to an alternate notation that already implement a scsyndef writer (stsc3 prints to _hsc3_ notation)

However:

1. _blksc3_ is a simpler mechansim for implementing a _notation_ editor

# Notation

The _stsc3_ implementation is currently written in Smalltalk _FileOut_ notation.
Other possibilities are _.som_ and _.stc_ notations.

# Comments

It would be nice to allow stored class and method comments written at the obvious locations.

~~~
  | OpenComment
  | CloseComment
...
  "/*"                                   { \_ -> OpenComment }
  "*/"                                   { \_ -> CloseComment }
...
      '/*'            { OpenComment }
      '*/'            { CloseComment }
~~~

# Categories

Categories can be stored in a map keyed by method name.
This approach require all methods with the same name to be in the same category for each object they are implemented in.

# Cuis

- Install _Cuis_ following instructions at <https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/>
- Start _Cuis_ and install the _OSProcess_ package by evaluating _Feature require: 'OSProcess'_ in a workspace.
- Filein _stsc3/st/Sc3-Filein-For-Cuis.st_ (see _stsc3/st/Makefile_)
