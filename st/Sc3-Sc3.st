Object subclass: #Sc3 instanceVariableNames: '' classVariableNames: '' poolDictionaries: '' category: 'Sound-Sc3'!

Sc3 comment: 'SuperCollider 3'!

!Sc3 class methodsFor: 'server control'!
reset
    "Reset Sc synthesiser."
    'hsc3-scsynth reset' systemCommand
!
playHelpGraphFile: graphFile
    "Play help file."
    Sc3 playFile: (Sc3 stsc3HelpGraphFile: graphFile).
!!

!Sc3 class methodsFor: 'environment'!
stsc3Directory
    "Name of stsc3 directory."
    ^'/home/rohan/sw/stsc3'
!
stsc3File: fileName
    "Absolute file name of name given relative to the stsc3 directory."
    ^Sc3 stsc3Directory , '/' , fileName
!
stsc3HelpGraphFile: graphFileName
    "Absolute file name of name given relative to the help graph directory of stsc3."
    ^Sc3 stsc3File: 'help/graph/' , graphFileName
!!

!Sc3 class methodsFor: 'c-smalltalk'!
stcFileToSt: fileName
    "Translate named C-Smalltalk (.stc) file to Smalltalk (.st), returning translated text."
    | tmp cmd |
    tmp := '/tmp/stcFileToSt.st'.
    cmd := 'stsc3 translate stc st ' , fileName , ' ' , tmp.
    cmd systemCommand.
    (Delay forMilliseconds: 250) wait. "..."
    ^tmp readTextFile.
!
stcToSt: stcText
    "Translate .stc text to .st text."
    | tmp |
    tmp := '/tmp/stcToSt.st'.
    tmp writeTextFile: [ :fd | stcText putOn: fd ].
    ^Sc3 stcFileToSt: tmp
!
evalStOrStcFile: fileName
    "Evaluate an expression read from a file.  The file may be either a Smalltalk (.st) or a C-Smalltalk (.stc) file"
    | fileText |
    fileText := (fileName endsWith: '.stc') ifTrue: [Sc3 stcFileToSt: fileName] ifFalse: [fileName readTextFile].
    ^fileText evalString
  !
evalText: text fromFile: fileName
    "Evaluate text, deciding if it needs to be translated from C-Smalltalk based on the name of the file the text is from"
    (fileName endsWith: '.stc') ifTrue: [^(Sc3 stcToSt: text) evalString] ifFalse: [^text evalString].
!
playFile: fileName
    "Play expression loaded from named file, which may be either an .st or .stc file."
    (Sc3 evalStOrStcFile: fileName) play
!
drawFile: fileName
    "Draw expression loaded from named file, which may be either an .st or .stc file."
    (Sc3 evalStOrStcFile: fileName) draw
!!
