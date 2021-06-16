stsc3 is initially stored here.

To use, [filein](http://wiki.squeak.org/squeak/1105) the below files to a Smalltalk image.

The largest file, [SC3-UGen.st](?t=stsc3&e=st/SC3-UGen.st), is auto-generated
using [mk.hs](?t=stsc3&e=hs/mk.hs)

Other files are:

[SC3-Base.st](?t=stsc3&e=st/SC3-Base.st):
General (neither music nor SC3 specific) classes and methods.

[SC3-Core.st](?t=stsc3&e=st/SC3-Core.st):
UGen and math operator classes, related core classes (mce, mrg, &etc).

[SC3-Env.st](?t=stsc3&e=st/SC3-Env.st):
Envelope classes Env and EnvBuilder and related classes (EnvPerc, EnvLinen, EnvASR &etc.)

[SC3-Math.st](?t=stsc3&e=st/SC3-Math.st) &
[SC3-Random.st](?t=stsc3&e=st/SC3-Random.st):
Methods that extend numerical classes and sequence classes.

[SC3-UGen-Composite.st](?t=stsc3&e=st/SC3-UGen-Composite.st):
UGens (ie. `LocalBuf` and `Splay`) that are not at `SC3-UGen.st`.

[SC3-UI.st](?t=stsc3&e=st/SC3-UI.st):
User-interface classes and methods.

[SC3-Haskell.st](?t=stsc3&e=st/SC3-Haskell.st):
Classes to write UGen graphs as expressions to text files for
interpretation by [hsc3](?t=hsc3).  The graphs are
written without proxied outputs, without applying
multiple-channel-expansion, and without applying rate-control editing.
The writer generates let bindings for all UGens.

[SC3-Squeak.st](?t=stsc3&e=st/SC3-Squeak.st) &
[SC3-Pharo.st](?t=stsc3&e=st/SC3-Pharo.st) &
[SC3-Squeak.st](?t=stsc3&e=st/SC3-Squeak.st) &
[SC3-Gnu.st](?t=stsc3&e=st/SC3-Gnu.st) :
Methods that cannot be written to work under all of Squeak, Pharo, Cuis and GNU-Smalltalk
have separate definitions.

The program `hsc3-graphs` is required to communicate with `scsynth`.
It is a part of the `hsc3-graphs` archive at <https://gitlab.com/rd--/hsc3-graphs>.
It depends upon [hsc3](https://hackage.haskell.org/package/hsc3),
[hsc3-dot](https://gitlab.com/rd--/hsc3-dot),
[hsc3-lisp](https://gitlab.com/rd--/hsc3-lisp) and
[hmt](https://gitlab.com/rd--/hmt).

![](sw/stsc3/lib/png/squeak-mouse.png)

* * *

There was previously a [SC3-Lisp.st](?t=stsc3&e=st/SC3-Lisp.st) file that is now deleted.
It wrote graphs for [rsc3](?t=rsc3).
