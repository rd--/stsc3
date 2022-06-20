stsc3 was initially stored here.  It is now a stored at [../som](http://rohandrape.net/?t=stsc3&e=som/README.md).

To use run _make_ and then [filein](http://wiki.squeak.org/squeak/1105) the appropriate file to a Smalltalk image.

The generated files are not stored in version control and the build stage requires Haskell.

![](sw/stsc3/lib/png/squeak-mouse.png)

* * *

Squeak:

Graph drawing isn't implemented directly, it requires:

- MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/OSProcess'

Cuis:

Visit the following files in the FileBrowser and select `Install Package`

- https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev/blob/master/CompatibilityPackages/SqueakCompatibility.pck.st
- https://github.com/Cuis-Smalltalk/OSProcess/blob/master/OSProcess.pck.st

* * *

Notes:

- stream << items =  items putOn: stream
- item putOn: stream = stream nextPut: item
