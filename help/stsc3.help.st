'/home/rohan/sw/stsc3/help/' fileListOn "open file browser on named directory"
'/home/rohan/sw/stsc3/help/stsc3.help.st' workspaceOn "open Workspace on named file"

Sc3Browser openOn: '/home/rohan/sw/stsc3/help/' "open ugen graph browser on stsc3/help"
Sc3Browser openOn: '/home/rohan/sw/hsc3-graphs/db/' "open ugen graph browser on the hsc3-graphs database"

"Help Graphs"

Browser newOnClass: Sc3HelpGraph class.
Sc3HelpGraph new f0Tw1395878538297892865 play.
Sc3 reset.

"(Cmd-b to browse, graphs are class methods)"
Sc3HelpGraph

"(Ctl-m to graph name to select and see implemetors of it)"
f0Tw1395878538297892865

"Envelope"

EnvSpec new curveNameDictionary "curve name dictionary"
(EnvSpec apply: #(#(0 1 0) #(0.1 0.9)  #lin nil nil 0)) coordinateArrayForEnvGen = #(0 2 -99 -99 1 0.1 1 0 0 0.9 1 0 )

"Ugen"

SinOsc new inspect "initialize sets default values for parameters"
SinOsc freq: 440 phase: 0 "freq:phase: is a class method"
SinOsc new freq: 440 ; phase: 0 "freq: and phase: are instance methods and ; is the cascade operator"
SinOsc primaryFactoryMethod = #freq:phase: "objects may indicate a primary factory method"
SinOsc apply: #(220 0.5) "perform:withArguments: at the primary factory method, in C-Smalltalk notation SinOsc(220, 0.5) translates to this"
PinkNoise primaryFactoryMethod = #new "the primary factory method may be new"
PinkNoise apply: #() "in C-Smalltalk notation PinkNoise() translates to this"
SinOsc perform: #freq:phase: withArguments: #(220 1)
SinOsc perform: #freq:phase:mul:add: withArguments: #(220 1 1 0)
WhiteNoise new hPZ1 pan2: 0 level: 0.05
WhiteNoise new pan2: (SinOsc freq: 0.1 phase: 0) level: 0.05

"Klank"

(KlankSpec freq: #(220 330 440) amp: nil time: nil) = #(220 1 1 330 1 1 440 1 1)
#((220 330 440) nil nil) asKlangSpec = #(220 1 1 330 1 1 440 1 1)

"File I/O - Text - Squeak, Gnu, Cuis"

'/tmp/str.text' writeTextFile: [:fd | 5.0 asString putOn: fd].
'/tmp/str.text' readTextFile.

"File I/O - Directory - Squeak, Gnu, Cuis"

'/home/rohan/sw/stsc3/help/' dirAllFilesWithExt: '.st'. "recursive directory listing by file extension"

"Evaluate - Squeak, Gnu, Cuis"

'1 + 1' evalString = 2 "=> true ; evaluate string"
'/home/rohan/sw/stsc3/help/graph/jmcc-why-supercollider.st' evalFile play "evaluate text file"

"Sc3"

Sc3 reset "C-c C-."

"UI"

'/home/rohan/sw/stsc3/lib/png/smalltalk-balloon.png' viewImageFile. "viewer for image file"
'/home/rohan/sw/stsc3/lib/png/squeak-mouse.png' viewImageFile. "viewer for image file"
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' viewTextFile. "editor for text file ; play, stop and draw buttons"

"Mouse ; Cursor ; Hand"

World activeHand position. "get mouse position"

"Browser"

Browser openBrowser. "open Browser"
Browser newOnCategory: 'Sound-Sc3'. "open Browser on Category"
Browser newOnClass: Ugen. "open Browser on Class"
Browser newOnClass: Ugen messageCategory: 'arithmetic'. "open Browser on Protocol of Class"
Browser newOnClass: AbstractUgen selector: #kr. "open Browser on Method of Class"

"Transcript"

Transcript open. "=> opens Transcript ; MVC"
1 to: 5 do: [ :x | Transcript cr ; show: x ]. "=> prints to Transcript"

"Method Finder"

ToolBuilder open: SelectorBrowser new. "open Method Finder (Selector Browser) tool ; squeak ; MVC"
SelectorBrowser new open. "squeak ; MVC"

"Message Names"

MessageNames openMessageNames. "open Message Names tool"

"Precedence"

2 + 5 * 5. "=> 35"
2 + (5 * 5). "=> 27"
2 + 5 min: 5.  "=> 5"
2 + (5 min: 5). "=> 7"
1 min: 2 max: 3. "=> 3"
(1 min: 2) max: 3. "=> 3"
1 min: (2 max: 3). "=> 1"

"Boolean"

(0.5 coin) if: [ 'p' ] value: [ 'q' ] "=> p or q with equal probability"

"Random - Float"

5.0 rand "=> uniform random Float in 0 to 5"
2.0 rand: 3.0 "=> uniform random Float in 2 to 3"
4.0 rand2 "=> uniform random Float in -4 to 4"
1.0 exprand: 2.0 "=> exponential distribution"
0.2 rand + 0.1 "=> uniform random Float in 0.1 to 0.3"

100 arrayFill: [ 5.0 rand ] "=> 100 random Floats in 0 to 5"
100 arrayFill: [ 0.2 coin ] "=> true 20%, false 80%"

"Random - Integer"

5 rand "=> random Integer in 1 to 5 ; i.e. atRandom"
1 rand: 5 "=> random Integer in 1 to 5"
4 rand2 "=> random Integer in -4 to 4"

100 arrayFill: [ 5 rand] "=> 100 random Integers in 0 to 5"

"Random - Array"

#(1 2 3 4 5) atRandom "random element of Array ; i.e. atRandom ; choose"
#(1 2 3 4 5) choose "alias for atRandom"
#(1 2 3 4 5) shuffled "=> i.e. copy shuffle"
100 arrayFill: [ #(1 2 3 4 5) atRandom] "=> 100 randomly chosen numbers"
100 arrayFill: [ #('a' 'b' 'c' 'd' 'e') atRandom] "=> 100 randomly chosen strings"

"Interval - Random"

9 arrayFill: [ (1 to: 9) atRandom ] "=> array of 9 integers between 1 and 9"
9 arrayFill: [ (1 to: 9) choose ] "alias for atRandom"

"Array"

#(1 2 3 4 5) "literal array syntax ; {1. 2. 3. 4. 5}"
#(true false nil) "=> #(true false nil)"
#(#(1 2) #(3 4)) "literal array syntax ; {{1. 2}. {3. 4}}"
#(1 2 3 4 5) size = 5 "=> true"

#(1 2 3) with: #(4 5 6) collect: [:i :j | {i. j}] "=> #((1 4) (2 5) (3 6))"
6 replicate: 1 "=> #(1 1 1 1 1 1)"
#(#(1 2) #(3 4)) concatenation "=> #(1 2 3 4)"
#(#(1 2) #(3 4)) transpose "=> #((1 3) (2 4))"
#(#(1 2) #(3 4)) transpose concatenation "=> #(1 3 2 4)"
(0 to: 4) mixFill: [:x | x * x] "=> 30"
(1 to: 5) collect: [:x | x * x] "=> #(1 4 9 16 25)"
(0 to: 4) collect: [:x | x * x] "=> #(0 1 4 9 16)"

#(1 2 3 4 5) groupsOf: 2 "=>  #((1 2 ) (3 4))"
#(1 2 3 4 5 6) clump: 2 "=>  #((1 2 ) (3 4) (5 6))"
#(1 2 3) extendCyclically: 5 "=> #(1 2 3 1 2)"
#((1 2) (3 4 5)) extendToBeOfEqualSize "=> #((1 2 1 ) (3 4 5))"
#(1 2 3) withExtending: #(4 5) collect: [:x :y| x + y] "=> #(5 7 7) ; 1 + 4 and 2 + 5 and 3 + 4"
#(1 2 3) ++ #(4 5) "=> #(1 2 3 4 5)"
#(1 2 3) , #(4 5) "=> #(1 2 3 4 5)"

(Array with: 1) = #(1)
(Array with: 1 with: 2) = #(1 2)
(Array with: 1 with: 2 with: 3) = #(1 2 3)
(Array with: 1 with: 2 with: 3 with: 4) = #(1 2 3 4)
(Array with: 1 with: 2 with: 3 with: 4 with: 5) = #(1 2 3 4 5)

(Array with: 1 with: 2.3 with: 'str') = #(1 2.3 'str')

#(5 4 3 2 1) withIndexDo: [:e :i | {e. i} print].

Array series: 10 value: 0 value: 0.1 "=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)"
Array geom: 5 value: 1 value: 3 "=> (1 3 9 27 81)"
Array fill: 5 value: [10 rand] "=> 5 random numbers between 1 and 10"

#(1 2 3 4 5) nth: 3 "=> 3 ; Sc and St disagree about indexing, nth: is one-indexed at both"

10 iota "=> 10 element array from 1 to 10"
10 iota - 1 * 0.1 "=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)"

10 from: 1 by: 1 "=> 10 element array from 1 to 10 by 1"
3 from: 3 by: 6 "=> 3 element array from 3 to 15 by 6"

({1. 2. 3} at: 2 put: -2) == -2 true              "=> true (at:put: returns the element put)"

"Array - Math"

3 adaptToCollection: #(1 2 3) andSend: #*
#(1 2 3) * 3 "=> #(3 6 9)"

"Interval"

(Interval from: 5 to: 9) asArray "=> #(5 6 7 8 9)"
(Interval from: 5 to: 15 by: 2) asArray "=> #(5 7 9 11 13 15)"
(1 to: 5) collect: [:x | x * x] "=> #(1 4 9 16 25)"
(1 to: 9 by: 2) collect: [:x | x * x] "=> #(1 9 25 49 81)"

"Strings"

'file' , '.ext' = 'file.ext'. "=> true"
'\n' "=> '\n'"
'\\n' "=> '\\n'"
'\0' "=> '\0'"
'
x' "=> newline x"

"Symbols"

#x isSymbol = true. "=> true"

"Haskell - List"

(1 to: 5) first = 1 "=> 1 ; head"
(1 to: 5) last = 5 "=> 5"
(1 to: 5) size = 5 "=> 5 ; length"
(1 to: 5) collect: [:x | x * x] "=> #(1 4 9 16 25) ; map"
(1 to: 5) select: [:x | x > 3] "=> #(4 5) ; filter"
(1 to: 5) copyFrom: 2 to: 5 "=> #(2 3 4 5) ; drop"
(1 to: 5) at: 3 "=> 3 ; haskell=! ; one-indexed"

"Association"

#x -> 1 "=> Association key: #x value: 1"
Association key: #x value: 1 "=> #x -> 1"
0.1 -> 0.9

"Dictionary"

d := Dictionary new. d at: 'select' put: 'x' ; at: 'from' put: 'y' ; at: 'list' put: 'z'
d := Dictionary newFrom: {'select' -> 'x'. 'from' -> 'y'. 'list' -> 'z'}
d := Dictionary newFromAssociations: {'select' -> 'x'. 'from' -> 'y'. 'list' -> 'z'} "=> newFrom: will do this courtesy Collection>>associationsDo:"
d := Dictionary newFromPairs: {'select'. 'x'. 'from'. 'y'. 'list'. 'z'}

"Conditionals"

x := nil
x isNil ifTrue: ['x'] ifFalse: ['y'] "=> x"

"Collections"

#(1 2 3 4 5) sum = 15 "=> true"
#(1 2 3 4 5) product == (1 * 2 * 3 * 4 * 5) "=> true"

"Class"

'x' class = String. "true"
'x' class class = String class. "true"
'x' class class class = Metaclass. "true"
'x' class class class class = Metaclass class. "true"
'x' class class class class class = Metaclass. "true"

SinOsc name = #SinOsc. "true"
SinOsc class. "SinOsc class"
SinOsc class name. "nil ; 'SinOsc class'"
SinOsc new class name = #SinOsc. "true"
SinOsc class class name = #Metaclass. "true"
SinOsc class class class name. "nil ; 'Metaclass class'"
SinOsc class class class class name = #Metaclass. "true"

SinOsc superclass name = #Ugen. "true"
SinOsc superclass superclass name = #AbstractUgen. "true"
SinOsc superclass superclass superclass name = #Object. "true"
SinOsc superclass superclass superclass superclass. "nil ; ProtoObject"

SinOsc subclasses size = 0. "true"
Ugen subclasses size = 153. "true"

SinOsc methodDict. "Squeak"
SinOsc methodDictionary. "Gnu Smalltalk"

SinOsc inspect.
SinOsc class inspect.
SinOsc new inspect.

True class superclass = True superclass class "=> true"
Object superclass = nil                       "=> true ; GNU "
Object class superclass = Class               "=> true ; GNU"
Object superclass = ProtoObject               "=> true ; Squeak"
ProtoObject class superclass = Class          "=> true ; Squeak"

$c class = Character                          "=> true"

1 perform: #class withArguments: #() "=> SmallInteger"
1 perform: #class withArguments: #() inSuperclass: SmallInteger "=> SmallInteger"
1 perform: #class withArguments: #() inSuperclass: Integer "=> SmallInteger"

"Morph"

menu := PopUpMenu labelArray: #('circle' 'oval' 'square' 'rectangle' 'triangle') lines: #(2 4).
menu startUpWithCaption: 'Choose a shape'

(UpdatingStringMorph on: Time selector: #now) openInHand. "clock"

"UI"

ListChooser chooseItemFrom: #(select an item from an array) title: 'Select:' addAllowed: true
ListChooser chooseIndexFrom: #(select an item from an array) title: 'Select:' addAllowed: true

ListMultipleChooser chooseItemListFrom: #(select an item from an array) title: 'Select:'
ListMultipleChooser chooseIndexListFrom: #(select an item from an array) title: 'Select:'

DirectoryChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') label: 'Select:'
FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') pattern: '*.st' label: 'Select:'
FileSaverDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/') initialFilename: 'x.st' label: 'Select:'

Project uiManager chooseDirectory
Project uiManager chooseFrom: #(select from list) "few items = buttons"
Project uiManager chooseFrom: ((100 to: 125) asArray) "many items = list"
Project uiManager confirm: 'really?'
Project uiManager confirm: 'really?' orCancel: [Transcript show: 'cancel' ; cr]
Project uiManager edit: 'edit text'
Project uiManager edit: 'edit text' label: 'make changes?' accept: [:x | Transcript show: x ; cr]
Project uiManager inform: 'the information'
Project uiManager request: 'name?'
Project uiManager requestPassword: 'pwd?'

"squeak=yes pharo=no"
Project uiManager chooseClassOrTrait
Project uiManager chooseDirectoryFrom: (FileDirectory on: '/home/rohan/sw/stsc3/help/')
Project uiManager chooseFileMatching: nil
Project uiManager chooseFileMatchingSuffixes: #('st')
Project uiManager chooseFromLabeledValues: (Dictionary newFromPairs: #(select x from y list z))
Project uiManager chooseMultipleFrom: #(select multiple from list)
Project uiManager multiLineRequest: 'text' centerAt: Sensor peekPosition initialAnswer: 'type text' answerHeight: 600
Project uiManager saveFilenameRequest: 'save file' initialAnswer: 'x.st'

"pharo=yes squeak=no"
Project uiManager chooseDirectoryFrom: ('/home/rohan/sw/stsc3/help/' asFileReference)
Project uiManager chooseFullFileNameMatching: nil

"Squeak ; Projects"

Project current isMorphic.

"Ui ; Mvc"

(PopUpMenu labelArray: #('Yes' 'No') lines: #()) startUp.
(PopUpMenu labels: 'Yes\No' withCRs) startUp.
(PopUpMenu labelArray: #('Yes' 'No') lines: #()) startUpWithCaption: 'Are you hungry?'.
(PopUpMenu labelArray: ((1 to: 100) asArray collect: [ :item | item asString ]) lines: (5 to: 95 by: 5)) startUpWithCaption: 'A menu with 100 items'.

(SelectionMenu labelList: #('Yes' 'No') selections: #(#yes #no)) startUp
(SelectionMenu labelList: #('sin' 'cos' 'neg') lines: #(2) selections: #(#sin #cos #negated)) invokeOn: 0.7

FillInTheBlank request: 'Frequency' initialAnswer: '440'.

StandardFileMenu oldFileFrom: (FileDirectory forFileName: '/home/rohan/sw/stsc3/help/').

Transcript open. "=> opens Transcript"
SelectorBrowser new open.
ProcessBrowser open.
Workspace open.

"FileList"

FileList open. "opens FileList ; default directory"
FileList openInMVC. "squeak ; MVC"
FileList openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/'). "squeak ; morphic"
FileList openOn: '/home/rohan/sw/stsc3/help/'. "pharo"

"ProcessBrowser"

ProcessBrowser open. "open process browser ; MVC"

"Workspace"

Workspace open. "opens workspace ; Squeak ; MVC"
Workspace open contents: 'Workspace open .'. "pharo"
Workspace open openFile: '/home/rohan/sw/stsc3/help/stsc3.st'. "pharo"

"Message Names"

MessageNames openMessageNames.
ToolBuilder open: (MessageNames methodBrowserSearchingFor: 'freqscale').

'/tmp/hsc3.png' viewImageFile. "UI image file viewer"

"OS ; squeak"

MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/OSProcess'.
MCMcmUpdater updateFromRepository: 'http://www.squeaksource.com/CommandShell'.

"OS ; cuis"

Feature require: 'OSProcess'

"OS ; pharo (use LibC)"

Metacello new configuration: 'OSProcess'; repository: 'http://www.squeaksource.com/MetacelloRepository'; load
Metacello new configuration: 'CommandShell'; repository: 'http://www.squeaksource.com/MetacelloRepository'; load

OSProcess command: 'ls -l /etc'
OSProcess waitForCommand: 'echo sleeping...; sleep 3; echo I just slept for three seconds'

OSProcess outputOf: 'ls | cat | wc'
OSProcess editOutputOf: 'ps -axjf'

"Command Shell"

CrLfFileEditor openForFileNamed: '/home/rohan/sw/stsc3/README.md'.
CrLfFileEditor openOn: 'string to edit' withLabel: 'window label' inDirectory: FileDirectory default defaultFileName: 'default.txt'.

"Squeak"

(FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/graph/') suffixList: #(st) label: 'select st file') evalTextFile play. "file chooser on dir"

FileList2 modalFileSelectorForSuffixes: #(st) directory: (FileDirectory on: '/home/rohan/sw/stsc3/help/'). "file chooser on dir"

'a text' edit. "=> Project uiManager edit: 'edit text'"

Display writePNGfileNamed: '/tmp/display.png'. "squeak ; screenshot"

Form fromUser asMorph openInWorld. "squeak ; screenshot selection"

(ImageReadWriter formFromFileNamed: '/tmp/hsc3.png') asMorph openInHand. "squeak ; image display"

(Form fromFileNamed: '/tmp/hsc3.png') asMorph openInHand. "squeak ; image display"

"File I/O - Object"

d := {'a'->1. 'b'->2. 'c'->'3'} as: Dictionary.
FileStream forceNewFileNamed: '/tmp/dict.st' do: [:out | d storeOn: out]. "store"
FileStream oldFileNamed: '/tmp/dict.st' do:  [:in | Object readFrom: in]. "read"
FileStream stdout nextPutAll: 'Hello, World'; cr; flush. "write to stdout"

"Smalltalk"

(Smalltalk at: #SinOsc) = SinOsc. "true"

Smalltalk garbageCollect. "Squeak"
Smalltalk aboutThisSystem. "System Reporter"

"Math"

69 midiCps = 440 "=> true"
440 cpsMidi = 69 "=> true"
-32 dbAmp = 0.025118864315095794 "=> true"
-32 dbAmp ampDb rounded = -32 "=> true"
0 raisedTo: 1 "=> 0"
1 raisedTo: 0 "=> 1"
0 raisedTo: 0 "=> 1 (gst = error)"
0.0 raisedTo: 1.0 "=> 0.0"
1.0 raisedTo: 0.0 "=> 1.0"
0.0 raisedTo: 0.0 "=> 1.0"
1 / 0 "=> error"
1.0 / 0.0 "=> Inf"
Float pi "=> 3.141592653589793"
Float pi * 2 "=> 6.283185307179586"
[:x | x * x] value: Float pi * 2 "=> 39.47841760435743"
Float infinity "=> Inf"
-1 raisedTo: (1/2). "=> ArithmeticError: Negative numbers don't have even roots. c.f. nthRoot:"
-1 raisedTo: (1/3). "=> -1 (Squeak)"
-1 raisedTo: 0.5. "=> ArithmeticError: Negative can't be raised to float powers."
(0.5 linExpFrom: 1 to: 2) = 1.414213562373095
(0.5 linExpFrom: 1 to: 2) = 1.414213562373095
(0 to: 10) collect: [ :n | (n / 10) linExpFrom: 1 to: 2 ] "=> 10 point equally spaced points on exponential curve from 1 to 2"

"GNU Smalltalk"

Smalltalk system: 'ls /home/rohan/sw/stsc3/README.md'.
Smalltalk getenv: 'HOME'. "/home/rohan"

ObjectMemory snapshot. "save image file (to current image file)"
ObjectMemory snapshot: '/tmp/stsc3.img'. "save image file (to indicated image file)"
ObjectMemory compact. "garbage collection (major)"
ObjectMemory scavenge. "garbage collection (minor)"
ObjectMemory current inspect. "vm memory statistics"
File image asString. "location of image file"

FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Filein-For-Gnu.st'.

FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Base.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Core.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Env.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Event.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Math.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Random.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Ugen.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Ugen-Composite.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Ugen-Demand.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Haskell.st'.

FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Gnu.st'.

FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Ugen-Filter.st'.

"Help Graphs"

Sc3 playHelpGraphFile: 'adc-bit-reduction.st'
Sc3 playHelpGraphFile: 'adc-down-sample.st'
Sc3 playHelpGraphFile: 'es-tw-435684664200540161.st'
Sc3 playHelpGraphFile: 'es-tw-570012853274615808.st'
Sc3 playHelpGraphFile: 'f0-pkt-00.st'
Sc3 playHelpGraphFile: 'f0-tw-0011.st'
Sc3 playHelpGraphFile: 'f0-tw-0041.st'
Sc3 playHelpGraphFile: 'f0-tw-0045.st'
Sc3 playHelpGraphFile: 'f0-tw-0077.st'
Sc3 playHelpGraphFile: 'f0-tw-0084.st'
Sc3 playHelpGraphFile: 'f0-tw-0121.st'
Sc3 playHelpGraphFile: 'f0-tw-0125.st'
Sc3 playHelpGraphFile: 'f0-tw-0456384156159574016.st'
Sc3 playHelpGraphFile: 'f0-tw-0839296011982024704.st'
Sc3 playHelpGraphFile: 'f0-tw-1105496695637454848.st'
Sc3 playHelpGraphFile: 'f0-tw-1125557217086857216.st'
Sc3 playHelpGraphFile: 'f0-tw-1210118661022867458.st'
Sc3 playHelpGraphFile: 'f0-tw-1254441448327479299.st' "<= multiple graphs"
Sc3 playHelpGraphFile: 'f0-tw-1338987643057688579.st'
Sc3 playHelpGraphFile: 'f0-tw-1343283697840226307.st'
Sc3 playHelpGraphFile: 'f0-tw-1395040511795372038.st'
Sc3 playHelpGraphFile: 'f0-tw-456384156159574016.st'
Sc3 playHelpGraphFile: 'jar-1-4Qx.st'
Sc3 playHelpGraphFile: 'jar-rk_20120422.st'
Sc3 playHelpGraphFile: 'jh-4157.st'
Sc3 playHelpGraphFile: 'jl-1-9.st'
Sc3 playHelpGraphFile: 'jl-bitwise.st'
Sc3 playHelpGraphFile: 'jl-rain-thunder.st'
Sc3 playHelpGraphFile: 'jmcc-analog-bubbles-mouse.st'
Sc3 playHelpGraphFile: 'jmcc-analog-bubbles.st'
Sc3 playHelpGraphFile: 'jmcc-analogue-daze.st'
Sc3 playHelpGraphFile: 'jmcc-babbling-brook.st'
Sc3 playHelpGraphFile: 'jmcc-berlin-1977.st'
Sc3 playHelpGraphFile: 'jmcc-bidirectional-strummable-guitar.st'
Sc3 playHelpGraphFile: 'jmcc-birdies.st'
Sc3 playHelpGraphFile: 'jmcc-demanding-studies.st'
Sc3 playHelpGraphFile: 'jmcc-harmonic-swimming.st'
Sc3 playHelpGraphFile: 'jmcc-harmonic-tumbling.st'
Sc3 playHelpGraphFile: 'jmcc-harmonic-zither.st'
Sc3 playHelpGraphFile: 'jmcc-inharmonic-warbulence.st'
Sc3 playHelpGraphFile: 'jmcc-lfo-modulation.st'
Sc3 playHelpGraphFile: 'jmcc-modal-space.st'
Sc3 playHelpGraphFile: 'jmcc-moto-rev.st'
Sc3 playHelpGraphFile: 'jmcc-narrow-band-filtered-crackle-noise.st'
Sc3 playHelpGraphFile: 'jmcc-plucked-strings.st'
Sc3 playHelpGraphFile: 'jmcc-pulsing-bottles-ir.st'
Sc3 playHelpGraphFile: 'jmcc-pulsing-bottles.st'
Sc3 playHelpGraphFile: 'jmcc-random-pulsations.st'
Sc3 playHelpGraphFile: 'jmcc-random-sine-waves.st'
Sc3 playHelpGraphFile: 'jmcc-reverberated-sine-percussion.st'
Sc3 playHelpGraphFile: 'jmcc-sample-and-hold-liquidities.st'
Sc3 playHelpGraphFile: 'jmcc-scratchy.st'
Sc3 playHelpGraphFile: 'jmcc-sprinkler-mouse.st'
Sc3 playHelpGraphFile: 'jmcc-sprinkler.st'
Sc3 playHelpGraphFile: 'jmcc-strummable-guitar.st'
Sc3 playHelpGraphFile: 'jmcc-strummable-silk.st'
Sc3 playHelpGraphFile: 'jmcc-sweepy-noise.st'
Sc3 playHelpGraphFile: 'jmcc-synthetic-piano.st'
Sc3 playHelpGraphFile: 'jmcc-tank.st'
Sc3 playHelpGraphFile: 'jmcc-theremin.st'
Sc3 playHelpGraphFile: 'jmcc-uplink.st'
Sc3 playHelpGraphFile: 'jmcc-why-supercollider.st'
Sc3 playHelpGraphFile: 'jrhb-gcd.st'
Sc3 playHelpGraphFile: 'nc-sctutorial.st'
Sc3 playHelpGraphFile: 'nv-899-04.st'
Sc3 playHelpGraphFile: 'nv-tw-40.st'
Sc3 playHelpGraphFile: 'nv-tw-41.st'
Sc3 playHelpGraphFile: 'pj-forest-sounds.st'
Sc3 playHelpGraphFile: 'rd-chrd.st'
Sc3 playHelpGraphFile: 'rd-eggcrate.st'
Sc3 playHelpGraphFile: 'rd-xy-interference.st'
Sc3 playHelpGraphFile: 'tb-1-4Tw.st'
Sc3 playHelpGraphFile: 'tw-tw-01.st'
Sc3 playHelpGraphFile: 'tw-tw-02.st' "<= multiple graphs"

Sc3 reset.

"Stc"
Sc3 stcToSt: 'p.q(r)'.
Sc3 stcFileToSt: (Sc3 stsc3HelpGraphFile: 'f0-tw-0456384156159574016.stc').
Sc3 playHelpGraphFile: 'f0-tw-0456384156159574016.stc'.
Sc3 reset.

"Blocks"
| factorial |
factorial := [:n| n = 1 ifTrue: [1] ifFalse: [(factorial value: n - 1) * n]].
(1 to: 10) collect: factorial "=> #(1 2 6 24 120 720 5040 40320 362880 3628800)"
[10 rand] dup: 10 "=> ten random numbers between 1 and 10"
[10 rand] dup "=> two random numbers between 1 and 10"
[] class = BlockClosure
[ :x | x := 2. x] value: 1 "=> 2 ; Gnu Smalltalk disallows assignment to arguments"
([ 1 ] cull: 2) == 1 "=> true"
([ :x | x ] cull: 2) == 2 "=> true"
([ :x :y | x * y ] valueWithArguments: #(3 4)) == 12 "=> true"
([ :x :y :z | x * y + z ] apply: #(3 4 5)) == 17 "=> alias for valueWithArguments:"

"Delay"
(Delay forMilliseconds: 500) wait

"Open Sound Control - Fudi"
m := OscMessage at: '/n_set' with: #(-1 'freq' 440 'amp' 0.1)
m inspect
m descriptor
m putOscOn: stdout
m putFudiOn: stdout

1234 putOscOn: stdout "must be in network byte order"
3.14 putOscOn: stdout "must be in network byte order"
'rq' putOscOn: stdout "must be 4-byte aligned and nul terminated"
#(1 2 3) asByteArray putOscOn: stdout "must be length prefixed in network order and 4-byte aligned"
