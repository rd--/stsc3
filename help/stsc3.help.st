'/home/rohan/sw/stsc3/help/' fileListOn "open file browser on named directory"
'/home/rohan/sw/stsc3/help/stsc3.help.st' workspaceOn "open Workspace on named file"

SC3Browser openOn: '/home/rohan/sw/stsc3/help/' "open ugen graph browser on stsc3/help"

"Envelope"

Env curveNameDictionary "curve name dictionary"

"UGen"

SinOsc freq: 440 phase: 0 "freq:phase: is a class method"
SinOsc new freq: 440 ; phase: 0 "freq: and phase: are instance methods and ; is the cascade operator"
SinOsc new inspect "initialize sets default values for parameters"

SinOsc ar: {220. Float pi}
SinOsc ar: {#freq: -> 110. #phase: -> (Float pi / 4)}
SinOsc ar: {#phase: -> 1. #freq: -> 55}
SinOsc ar: {22.5. #phase: -> 0}
BrownNoise ar

"File I/O - Text"

'/tmp/str.text' writeTextFile: [:fd | 5.0 asString putOn: fd].
'/tmp/str.text' readTextFile.

"File I/O - Directory"

'/home/rohan/sw/stsc3/help/' dirAllFilesWithExt: '.st'. "recursive directory listing by file extension"

"Evaluate"

'1+1' evalString = 2 "=> true ; evaluate string"
'/home/rohan/sw/stsc3/help/graph/jmcc-why-supercollider.st' evalFile play "evaluate text file"

"SC3"

SC3 reset "C-c C-."

"UI"

'/home/rohan/sw/stsc3/lib/png/smalltalk-balloon.png' viewImageFile. "viewer for image file"
'/home/rohan/sw/stsc3/lib/png/squeak-mouse.png' viewImageFile. "viewer for image file"
'/home/rohan/sw/stsc3/help/graph/jmcc-analog-bubbles.st' viewTextFile. "editor for text file ; play, stop and draw buttons"

"Mouse ; Cursor ; Hand"

World activeHand position. "get mouse position"

"Browser"

Browser openBrowser. "open Browser"
Browser newOnCategory: 'SC3'. "open Browser on Category"
Browser newOnClass: UGen. "open Browser on Class"
Browser newOnClass: UGen messageCategory: 'rate control'. "open Browser on Protocol of Class"
Browser newOnClass: UGen selector: #kr. "open Browser on Method of Class"

"Transcript"

Transcript open. "=> opens Transcript ; MVC"
1 to: 5 do: [:x | Transcript cr ; show: x]. "=> prints to Transcript"

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

"Random - Float"

5.0 rand "=> uniform random Float in 0 to 5"
2.0 rand: 3.0 "=> uniform random Float in 2 to 3"
4.0 rand2 "=> uniform random Float in -4 to 4"
1.0 exprand: 2.0 "=> exponential distribution"

100 arrayFill: [:i | 5.0 rand] "=> 100 random Floats in 0 to 5"

"Random - Integer"

5 rand "=> random Integer in 1 to 5 ; i.e. atRandom"
1 rand: 5 "=> random Integer in 1 to 5"
4 rand2 "=> random Integer in -4 to 4"

100 arrayFill: [:i | 5 rand] "=> 100 random Integers in 0 to 5"

"Random - Array"

#(1 2 3 4 5) atRandom "random element of Array ; i.e. atRandom ; choose"
#(1 2 3 4 5) shuffled "=> i.e. copy shuffle"
100 arrayFill: [:i | #(1 2 3 4 5) atRandom] "=> 100 randomly chosen numbers"
100 arrayFill: [:i | {#a. #b. #c. #d. #e} atRandom] "=> 100 randomly chosen symbols"

"Array"

#(1 2 3 4 5) "literal array syntax ; {1. 2. 3. 4. 5}"
#(true false nil) "=> #(true false nil)"
#(#(1 2) #(3 4)) "literal array syntax ; {{1. 2}. {3. 4}}"
#(1 2 3 4 5) size = 5 "=> true"

#(1 2 3) with: #(4 5 6) collect: [:i :j | {i. j}] "=> #((1 4) (2 5) (3 6))"
6 replicate: 1 "=> #(1 1 1 1 1 1)"
#(#(1 2) #(3 4)) concatenation "=> #(1 2 3 4)"
#(#(1 2) #(3 4)) transpose concatenation "=> #(1 3 2 4)"
5 mixFill: [:x | x * x] "=> 30 ; mixFill is one indexed (as in smalltalk)"
5 arrayFill: [:x | x * x] "=> #(1 4 9 16 25)"
5 arrayFillZeroIndexed: [:x | x * x] "=> #(0 1 4 9 16)"

(Array with: 1) = #(1)
(Array with: 1 with: 2) = #(1 2)
(Array with: 1 with: 2 with: 3) = #(1 2 3)
(Array with: 1 with: 2 with: 3 with: 4) = #(1 2 3 4)
(Array with: 1 with: 2 with: 3 with: 4 with: 5) = #(1 2 3 4 5)

(Array with: 1 with: 2.3 with: 'str') = #(1 2.3 'str')

#(5 4 3 2 1) withIndexDo: [:e :i | {e. i} print].

10 arraySeries: 0 step: 0.1 "=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)"

10 iota - 1 * 0.1 "=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)"

5 arrayGeom: 1 grow: 3 "=> (1 3 9 27 81)"

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

"Dictionary"

d := Dictionary new. d at: 'select' put: 'x' ; at: 'from' put: 'y' ; at: 'list' put: 'z'
d := Dictionary newFrom: {'select' -> 'x'. 'from' -> 'y'. 'list' -> 'z'}
d := Dictionary newFromPairs: {'select'. 'x'. 'from'. 'y'. 'list'. 'z'}

"Conditionals"

x := nil
x isNil ifTrue: ['x'] ifFalse: ['y'] "=> x"

"Collections"

#(1 2 3 4 5) sum = 15 "=> true"

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

SinOsc superclass name = #UGen. "true"
SinOsc superclass superclass name = #AbstractUGen. "true"
SinOsc superclass superclass superclass name = #Object. "true"
SinOsc superclass superclass superclass superclass. "nil ; ProtoObject"

SinOsc subclasses size = 0. "true"
UGen subclasses size = 104. "true"

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

UIManager default chooseDirectory
UIManager default chooseFrom: #(select from list) "few items = buttons"
UIManager default chooseFrom: ((100 to: 125) asArray) "many items = list"
UIManager default confirm: 'really?'
UIManager default confirm: 'really?' orCancel: [Transcript show: 'cancel' ; cr]
UIManager default edit: 'edit text'
UIManager default edit: 'edit text' label: 'make changes?' accept: [:x | Transcript show: x ; cr]
UIManager default inform: 'the information'
UIManager default request: 'name?'
UIManager default requestPassword: 'pwd?'

"squeak=yes pharo=no"
UIManager default chooseClassOrTrait
UIManager default chooseDirectoryFrom: (FileDirectory on: '/home/rohan/sw/stsc3/help/')
UIManager default chooseFileMatching: nil
UIManager default chooseFileMatchingSuffixes: #('st')
UIManager default chooseFromLabeledValues: (Dictionary newFromPairs: #(select x from y list z))
UIManager default chooseMultipleFrom: #(select multiple from list)
UIManager default multiLineRequest: 'text' centerAt: Sensor peekPosition initialAnswer: 'type text' answerHeight: 600
UIManager default saveFilenameRequest: 'save file' initialAnswer: 'x.st'

"pharo=yes squeak=no"
UIManager default chooseDirectoryFrom: ('/home/rohan/sw/stsc3/help/' asFileReference)
UIManager default chooseFullFileNameMatching: nil

"FileList"

FileList open. "opens FileList ; default directory"
FileList openInMVC. "squeak ; MVC"
FileList openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/'). "squeak"
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

"Squeak"

(FileChooserDialog openOn: (FileDirectory on: '/home/rohan/sw/stsc3/help/graph/') suffixList: #(st) label: 'select st file') evalTextFile play. "file chooser on dir"

FileList2 modalFileSelectorForSuffixes: #(st) directory: (FileDirectory on: '/home/rohan/sw/stsc3/help/'). "file chooser on dir"

'a text' edit. "=> UIManager default edit: 'edit text'"

Display writePNGfileNamed: '/tmp/display.png'. "squeak ; screenshot"

Form fromUser asMorph openInWorld. "squeak ; screenshot selection"

(ImageReadWriter formFromFileNamed: '/tmp/hsc3.png') asMorph openInHand. "squeak ; image display"

(Form fromFileNamed: '/tmp/hsc3.png') asMorph openInHand. "squeak ; image display"

"File I/O - Object"

d := {'a'->1. 'b'->2. 'c'->'3'} as: Dictionary.
FileStream forceNewFileNamed: '/tmp/dict.st' do: [:out | d storeOn: out]. "store"
FileStream oldFileNamed: '/tmp/dict.st' do:  [:in | Object readFrom: in]. "read"

"Smalltalk"

(Smalltalk at: #SinOsc) = SinOsc. "true"

Smalltalk garbageCollect. "Squeak"
Smalltalk aboutThisSystem. "System Reporter"

"Math"

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
-1 raisedTo: (1/2). "=> ArithmeticError: Negative numbers don't have even roots. c.f. nthRoot:"
-1 raisedTo: (1/3). "=> -1 (Squeak)"
-1 raisedTo: 0.5. "=> ArithmeticError: Negative can't be raised to float powers."

"GNU Smalltalk"

Smalltalk system: 'ls /home/rohan/sw/stsc3/README.md'.
Smalltalk getenv: 'HOME'. "/home/rohan"

ObjectMemory snapshot. "save image file (to current image file)"
ObjectMemory snapshot: '/tmp/stsc3.img'. "save image file (to indicated image file)"
ObjectMemory compact. "garbage collection (major)"
ObjectMemory scavenge. "garbage collection (minor)"
ObjectMemory current inspect. "vm memory statistics"

FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Base.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Core.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Random.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Math.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-UGen.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-UGen-Composite.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Env.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Event.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Haskell.st'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/SC3-Gnu.st'.

"Help Graphs"

SC3 playNamedGraph: 'adc-bit-reduction'.
SC3 playNamedGraph: 'adc-down-sample'.
SC3 playNamedGraph: 'es-tw-435684664200540161'.
SC3 playNamedGraph: 'es-tw-570012853274615808'.
SC3 playNamedGraph: 'f0-pkt-00'.
SC3 playNamedGraph: 'f0-tw-0011'.
SC3 playNamedGraph: 'f0-tw-0041'.
SC3 playNamedGraph: 'f0-tw-0045'.
SC3 playNamedGraph: 'f0-tw-0077'.
SC3 playNamedGraph: 'f0-tw-0084'.
SC3 playNamedGraph: 'f0-tw-0121'.
SC3 playNamedGraph: 'f0-tw-0125'.
SC3 playNamedGraph: 'f0-tw-0456384156159574016'.
SC3 playNamedGraph: 'f0-tw-0839296011982024704'.
SC3 playNamedGraph: 'f0-tw-1105496695637454848'.
SC3 playNamedGraph: 'f0-tw-1125557217086857216'.
SC3 playNamedGraph: 'f0-tw-1210118661022867458'.
SC3 playNamedGraph: 'f0-tw-1254441448327479299'. "<= multiple graphs"
SC3 playNamedGraph: 'f0-tw-1338987643057688579'.
SC3 playNamedGraph: 'f0-tw-1343283697840226307'.
SC3 playNamedGraph: 'f0-tw-1395040511795372038'.
SC3 playNamedGraph: 'f0-tw-456384156159574016'.
SC3 playNamedGraph: 'jar-1-4Qx'.
SC3 playNamedGraph: 'jar-rk_20120422'.
SC3 playNamedGraph: 'jh-4157'.
SC3 playNamedGraph: 'jl-1-9'.
SC3 playNamedGraph: 'jl-bitwise'.
SC3 playNamedGraph: 'jl-rain-thunder'.
SC3 playNamedGraph: 'jmcc-analog-bubbles-mouse'.
SC3 playNamedGraph: 'jmcc-analog-bubbles'.
SC3 playNamedGraph: 'jmcc-analogue-daze'.
SC3 playNamedGraph: 'jmcc-babbling-brook'.
SC3 playNamedGraph: 'jmcc-berlin-1977'.
SC3 playNamedGraph: 'jmcc-bidirectional-strummable-guitar'.
SC3 playNamedGraph: 'jmcc-birdies'.
SC3 playNamedGraph: 'jmcc-demanding-studies'.
SC3 playNamedGraph: 'jmcc-harmonic-swimming'.
SC3 playNamedGraph: 'jmcc-harmonic-tumbling'.
SC3 playNamedGraph: 'jmcc-harmonic-zither'.
SC3 playNamedGraph: 'jmcc-inharmonic-warbulence'.
SC3 playNamedGraph: 'jmcc-lfo-modulation'.
SC3 playNamedGraph: 'jmcc-modal-space'.
SC3 playNamedGraph: 'jmcc-moto-rev'.
SC3 playNamedGraph: 'jmcc-narrow-band-filtered-crackle-noise'.
SC3 playNamedGraph: 'jmcc-plucked-strings'.
SC3 playNamedGraph: 'jmcc-pulsing-bottles-ir'.
SC3 playNamedGraph: 'jmcc-pulsing-bottles'.
SC3 playNamedGraph: 'jmcc-random-pulsations'.
SC3 playNamedGraph: 'jmcc-random-sine-waves'.
SC3 playNamedGraph: 'jmcc-reverberated-sine-percussion'.
SC3 playNamedGraph: 'jmcc-sample-and-hold-liquidities'.
SC3 playNamedGraph: 'jmcc-scratchy'.
SC3 playNamedGraph: 'jmcc-sprinkler-mouse'.
SC3 playNamedGraph: 'jmcc-sprinkler'.
SC3 playNamedGraph: 'jmcc-strummable-guitar'.
SC3 playNamedGraph: 'jmcc-strummable-silk'.
SC3 playNamedGraph: 'jmcc-sweepy-noise'.
SC3 playNamedGraph: 'jmcc-synthetic-piano'.
SC3 playNamedGraph: 'jmcc-tank'.
SC3 playNamedGraph: 'jmcc-theremin'.
SC3 playNamedGraph: 'jmcc-uplink'.
SC3 playNamedGraph: 'jmcc-why-supercollider'.
SC3 playNamedGraph: 'jrhb-gcd'.
SC3 playNamedGraph: 'nc-sctutorial'.
SC3 playNamedGraph: 'nv-899-04'.
SC3 playNamedGraph: 'nv-tw-40'.
SC3 playNamedGraph: 'nv-tw-41'.
SC3 playNamedGraph: 'pj-forest-sounds'.
SC3 playNamedGraph: 'rd-chrd'.
SC3 playNamedGraph: 'rd-eggcrate'.
SC3 playNamedGraph: 'rd-xy-interference'.
SC3 playNamedGraph: 'tb-1-4Tw'.
SC3 playNamedGraph: 'tw-tw-01'.
SC3 playNamedGraph: 'tw-tw-02'.

SC3 reset.

"Blocks"
| factorial |
factorial := [:n| n = 1 ifTrue: [1] ifFalse: [(factorial value: n - 1) * n]].
(1 to: 10) collect: factorial "=> #(1 2 6 24 120 720 5040 40320 362880 3628800)"

"Som"
TestHarness new run: #('TestHarness.som').
TestHarness new run: #('TestHarness.som' 'ArrayTest').
TestHarness new run: #('TestHarness.som' 'BlockTest').
TestHarness new run: #('TestHarness.som' 'ClassLoadingTest').
TestHarness new run: #('TestHarness.som' 'ClassStructureTest').
TestHarness new run: #('TestHarness.som' 'ClosureTest').
TestHarness new run: #('TestHarness.som' 'CoercionTest').
TestHarness new run: #('TestHarness.som' 'CompilerReturnTest').
TestHarness new run: #('TestHarness.som' 'DoesNotUnderstandTest').
TestHarness new run: #('TestHarness.som' 'DoubleTest').
TestHarness new run: #('TestHarness.som' 'EmptyTest').
TestHarness new run: #('TestHarness.som' 'GlobalTest').
TestHarness new run: #('TestHarness.som' 'HashTest').
TestHarness new run: #('TestHarness.som' 'IntegerTest').
TestHarness new run: #('TestHarness.som' 'PreliminaryTest').
TestHarness new run: #('TestHarness.som' 'ReflectionTest').
TestHarness new run: #('TestHarness.som' 'SelfBlockTest').
TestHarness new run: #('TestHarness.som' 'SetTest').
TestHarness new run: #('TestHarness.som' 'SpecialSelectorsTest').
TestHarness new run: #('TestHarness.som' 'StringTest').
TestHarness new run: #('TestHarness.som' 'SuperTest').
TestHarness new run: #('TestHarness.som' 'SymbolTest').
TestHarness new run: #('TestHarness.som' 'SystemTest').
TestHarness new run: #('TestHarness.som' 'VectorTest').

"Micro Benchmarks"
Harness new run: #('Harness' 'Bounce').
Harness new run: #('Harness' 'List').
Harness new run: #('Harness' 'Permute').
Harness new run: #('Harness' 'Queens').
Harness new run: #('Harness' 'Sieve').
Harness new run: #('Harness' 'Storage').
Harness new run: #('Harness' 'Towers').

"Benchmarks"
Harness new run: #('Harness' 'CD' '1' '2'). "? Method cos not found in class Integer"
Harness new run: #('Harness' 'DeltaBlue' '1' '15').
Harness new run: #('Harness' 'Havlak' '1' '1')
Harness new run: #('Harness' 'Json').
Harness new run: #('Harness' 'Mandelbrot' '1' '1')
Harness new run: #('Harness' 'NBody' '1' '1')
Harness new run: #('Harness' 'NBody' '1' '250000')
Harness new run: #('Harness' 'Richards').
