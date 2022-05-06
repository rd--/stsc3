stsc3 is initially stored here.

To use, [filein](http://wiki.squeak.org/squeak/1105) the below files to a Smalltalk image.

The largest files,
[Sc3-Ugen-Bindings.st](?t=stsc3&e=st/Sc3-Ugen-Bindings.st) and
[Sc3-Ugen-Filter.st](?t=stsc3&e=st/Sc3-Ugen-Filter.st),
are auto-generated using [mk.hs](?t=stsc3&e=hs/mk.hs)

Other files are:

[Sc3-Base.st](?t=stsc3&e=st/Sc3-Base.st):
General (neither music nor Sc3 specific) classes and methods.

[Sc3-Core.st](?t=stsc3&e=st/Sc3-Core.st):
Ugen and math operator classes, related core classes (mce, mrg, &etc).

[Sc3-Env.st](?t=stsc3&e=st/Sc3-Env.st):
Envelope classes EnvSpec and EnvBuilder and related classes (EnvPerc, EnvLinen, EnvASR &etc.)

[Sc3-Event.st](?t=stsc3&e=st/Sc3-Event.st):
A simple event model with a Voicer class.

[Sc3-Math.st](?t=stsc3&e=st/Sc3-Math.st) &
[Sc3-Random.st](?t=stsc3&e=st/Sc3-Random.st):
Methods that extend numerical classes and sequence classes.

[Sc3-OpenSoundControl.st](?t=stsc3&e=st/Sc3-OpenSoundControl.st):
Open Sound Control protocol.

[Sc3-Ugen-Pseudo.st](?t=stsc3&e=st/Sc3-Ugen-Pseudo.st):
Pseudo Ugens (ie. _LocalBuf_ and _Splay2_) that are not at `Sc3-Ugen.st`.
This includes Demand Ugens constructors with parameter ordering consistent with _hsc3_.

[Sc3-Ui.st](?t=stsc3&e=st/Sc3-Ui.st):
User-interface classes and methods.

[Sc3-Haskell.st](?t=stsc3&e=st/Sc3-Haskell.st):
Classes to write Ugen graphs as expressions to text files for
interpretation by [hsc3](?t=hsc3).  The graphs are
written without proxied outputs, without applying
multiple-channel-expansion, and without applying rate-control editing.
The writer generates let bindings for all Ugens.

[Sc3-Squeak.st](?t=stsc3&e=st/Sc3-Squeak.st) &
[Sc3-Gnu.st](?t=stsc3&e=st/Sc3-Gnu.st) &
[Sc3-Cuis.st](?t=stsc3&e=st/Sc3-Cuis.st) &
[Sc3-Pharo.st](?t=stsc3&e=st/Sc3-Pharo.st):
Methods that cannot be written to work under all of Squeak, Gnu-Smalltalk, Cuis and Pharo have separate definitions.

The program `hsc3-graphs` is required to communicate with `scsynth`.
It is a part of the `hsc3-graphs` archive at <https://gitlab.com/rd--/hsc3-graphs>.
It depends upon [hsc3](https://hackage.haskell.org/package/hsc3),
[hsc3-dot](https://gitlab.com/rd--/hsc3-dot),
[hsc3-lisp](https://gitlab.com/rd--/hsc3-lisp) and
[hmt-base](https://gitlab.com/rd--/hmt-base).

![](sw/stsc3/lib/png/squeak-mouse.png)

* * *

There was previously a [Sc3-Lisp.st](?t=stsc3&e=st/Sc3-Lisp.st) file that is now deleted.
It wrote graphs for [rsc3](?t=rsc3).

* * *

Squeak:

- MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/OSProcess'
- MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/CommandShell'

Cuis:

Visit the following files in the FileBrowser and select `Install Package`

- https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/CompatibilityPackages/SqueakCompatibility.pck.st
- https://github.com/Cuis-Smalltalk/OSProcess/blob/master/OSProcess.pck.st

* * *

Notes:

- stream << items      =   items putOn: stream
- item putOn: stream   =   stream nextPut: item
