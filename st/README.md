stsc3 was initially stored here.  It is now a stored both here and at ../som.

To use, [filein](http://wiki.squeak.org/squeak/1105) the below files to a Smalltalk image.

The _Ui_ directory has user-interface classes and methods.

The _Cuis_, _Gnu_ and _Squeak_ directories have methods that cannot be written to work under all of Squeak, Gnu-Smalltalk, Cuis.

![](sw/stsc3/lib/png/squeak-mouse.png)

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

- stream << items =  items putOn: stream
- item putOn: stream = stream nextPut: item
