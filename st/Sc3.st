Object subclass: #Sc3
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Sc3 comment: 'SuperCollider 3'!

!Sc3 class methodsFor: 'haskell'!
ugenSetOf: anObject
    | set |
    set := IdentitySet new.
    anObject addToUgenSet: set.
    ^set!
writeHaskellFor: anObject To: aFileName
    "Write the hsc3 representation of anObject to a file."
    | set |
    set := Sc3 ugenSetOf: anObject.
    set asArray with: (1 to: set size) do: [ :item :index | item id: index ]. "<- assign id to Ugens for serialisation"
    aFileName writeTextFile: [ :fd |
     'let {' putOn: fd.
     set do: [:x | x printHaskellOn: fd. ' = ' putOn: fd. x printHaskellDefinitionOn: fd.] separatedBy: [fd cr. '; ' putOn: fd].
     '} in ' putOn: fd.
     anObject printHaskellOn: fd.
     fd cr]!
play: anObject
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to play me."
    Sc3 writeHaskellFor: anObject To: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min play /tmp/st.hs' systemCommand!
draw: anObject
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to draw me."
    self writeHaskellFor: anObject To: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min draw /tmp/st.hs' systemCommand!
printUgens: anObject
    "Write my hsc3 representation to a temporary file and run hsc3-graphs to print me."
    self writeHaskellFor: anObject To: '/tmp/st.hs'.
    'hsc3-graphs fragments hs min dump-ugens /tmp/st.hs' systemCommand!
!

!Sc3 class methodsFor: 'c-smalltalk'!
stcFileToSt: fileName
    "Translate named C-Smalltalk (.stc) file to Smalltalk (.st), returning translated text."
    | tmp cmd |
    tmp := '/tmp/stcFileToSt.st'.
    cmd := 'stsc3 translate stc st ' , fileName , ' ' , tmp.
    cmd systemCommand.
    (Delay forMilliseconds: 250) wait. "..."
    ^tmp readTextFile!
stcToSt: stcText
    "Translate .stc text to .st text."
    | tmp |
    tmp := '/tmp/stcToSt.st'.
    tmp writeTextFile: [ :fd | stcText putOn: fd ].
    ^Sc3 stcFileToSt: tmp!
evalStOrStcFile: fileName
    "Evaluate an expression read from a file.  The file may be either a Smalltalk (.st) or a C-Smalltalk (.stc) file"
    | fileText |
    fileText := (fileName endsWith: '.stc') ifTrue: [Sc3 stcFileToSt: fileName] ifFalse: [fileName readTextFile].
    ^fileText evalString!
evalText: text fromFile: fileName
    "Evaluate text, deciding if it needs to be translated from C-Smalltalk based on the name of the file the text is from"
    (fileName endsWith: '.stc') ifTrue: [^(Sc3 stcToSt: text) evalString] ifFalse: [^text evalString]!
playFile: fileName
    "Play expression loaded from named file, which may be either an .st or .stc file."
    (Sc3 evalStOrStcFile: fileName) play!
drawFile: fileName
    "Draw expression loaded from named file, which may be either an .st or .stc file."
    (Sc3 evalStOrStcFile: fileName) draw!
!

!Sc3 class methodsFor: 'environment'!
stsc3Directory
    "Name of stsc3 directory."
    ^'/home/rohan/sw/stsc3'!
stsc3File: fileName
    "Absolute file name of name given relative to the stsc3 directory."
    ^Sc3 stsc3Directory , '/' , fileName!
stsc3HelpGraphFile: graphFileName
    "Absolute file name of name given relative to the help graph directory of stsc3."
    ^Sc3 stsc3File: 'help/graph/' , graphFileName!
!

!Sc3 class methodsFor: 'server control'!
reset
    "Reset Sc synthesiser."
    'hsc3-scsynth reset' systemCommand!
playHelpGraphFile: graphFile
    "Play help file."
    Sc3 playFile: (Sc3 stsc3HelpGraphFile: graphFile)!
!
