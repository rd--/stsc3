'/home/rohan/sw/stsc3/help/' fileListOn "open file browser on named directory"
'/home/rohan/sw/stsc3/help/stsc3.help.st' workspaceOn "open Workspace on named file"

Sc3Browser openOn: '/home/rohan/sw/stsc3/help/' "open ugen graph browser on stsc3/help"
Sc3Browser openOn: '/home/rohan/sw/hsc3-graphs/db/' "open ugen graph browser on the hsc3-graphs database"

"Help Graphs"

Browser newOnClass: Sc3HelpGraph class.
Sc3HelpGraph new f0Tw1395878538297892865 play.
Sc3 reset.
Sc3HelpGraph methodDictionary size = 81.
(Sc3HelpGraph new perform: (Sc3HelpGraph methodDictionary keys atRandom)) play. "play random help graph"
Sc3 reset.

"(Cmd-b to browse, graphs are class methods)"
Sc3HelpGraph

"(Ctl-m to graph name to select and see implemetors of it)"
f0Tw1395878538297892865

"Apply"

SinOsc canUnderstand: #apply: "test if class can understand selector"
(SinOsc apply: #(440 0)) * 0.1

"Envelope"

{
(EnvSpec curveNameDictionary at: #exp) = 2. "curve name dictionary"
(EnvSpec apply: #(#(0 1 0) #(0.1 0.9)  #lin nil nil 0)) asArray = #(0 2 -99 -99 1 0.1 1 0 0 0.9 1 0 ).
} allSatisfy: #yourself

"Ugen"

{
(SinOsc freq: 440 phase: 0) isUgen. "freq:phase: is a class method"
(SinOsc freq: 440 phase: 0 mul: 0.1) isUgen. "freq:phase:mul: is a class method"
(SinOsc freq: 440 phase: 0 mul: 0.1 add: 0) isUgen. "freq:phase:mul: is a class method"
SinOsc primaryFactoryMethod = #freq:phase:. "objects may indicate a primary factory method"
(SinOsc apply: #(220 0.5)) isUgen. "perform:withArguments: at the primary factory method, in .stc notation SinOsc(220, 0.5) translates to this"
PinkNoise primaryFactoryMethod = #new. "the primary factory method may be new"
(PinkNoise apply: #()) isUgen. "in .stc notation PinkNoise() translates to this"
(SinOsc perform: #freq:phase: withArguments: #(220 1)) isUgen.
(SinOsc perform: #freq:phase:mul:add: withArguments: #(220 1 1 0)) isUgen.
} allSatisfy: #yourself

"UgenBuilder"

(UgenBuilder name: 'SinOsc' numChan: 1 rateSpec: 2 specialIndex: 0 inputArray: #(440 0)) isUgen.

"Graph"

u := SinOsc freq: 440 phase: Float pi mul: 0.1.
(ScGraph from: u withName: 'tmp') printString
(ScGraph ugenSetOf: u) do: [ :item | item inspect ]
Sc3 writeBinarySyndefOf: (Out bus: 0 channelsArray: (SinOsc freq: 440 phase: Float pi mul: 0.1)) to: '/tmp/stsc3.scsyndef'
ScGraph scgf = 1396926310.
((ScGraph scgf) digitAt: 4) = 83. "$S asInteger = 83"
((4 to: 1 by: -1) collect: [ :index | Character value: ((ScGraph scgf) digitAt: index) ]) = #($S $C $g $f).

"Splay2"

#(1 2 3) + 1 = #(2 3 4)
#(1 2 3) * 2 = #(2 4 6)
#(1 2 3) - 1 = #(0 1 2)
1 - #(1 2 3) = #(0 -1 -2)
#(1 2 3) / 2 = #(0.5 1 1.5)

"Scsynth"

Sc3 draw: (SinOsc freq: 440 phase: 0 mul: 0.1).
Sc3 play: (SinOsc freq: 440 phase: 0 mul: 0.1).
Sc3 playUsingTextSyndef: (SinOsc freq: 440 phase: 0 mul: 0.1).
Sc3 playUsingBinarySyndef: (SinOsc freq: 440 phase: 0 mul: 0.1).
Sc3 playUsingUdpDirectlyEncoded: (SinOsc freq: 440 phase: 0 mul: 0.1).
Sc3 playUsingUdpWithTemporaryFile: (SinOsc freq: 440 phase: 0 mul: 0.1).
Sc3 reset.

(SinOsc freq: 440 phase: 0 mul: 0.1) draw.
(SinOsc freq: 440 phase: 0 mul: 0.1) play.
(SinOsc freq: #(440 441) phase: 0 mul: 0.1) play.
Sc3 reset.

"Rate"

{
Rate ir = 0.
(Rate symbol: 0) = #ir.
[ Rate symbol: 7 ] on: Error do: [ true ].
} allSatisfy: #yourself

"Klank"

{
#((220 330 440) nil nil) asKlangSpec = #(220 1 1 330 1 1 440 1 1).
#((220 330 440) #(0.2 0.1) #(0.5)) asKlankSpec = #(220 0.2 0.5 330 0.1 0.5 440 0.2 0.5)
} allSatisfy: #yourself

"File I/O - Text - Squeak, Gnu, Cuis"

{
('/tmp/str.text' writeTextFile: [ :fd | 5.0 asString putOn: fd ]) = '/tmp/str.text'.
'/tmp/str.text' readTextFile = '5.0'.
'/tmp/str.text' evalFile = 5.0.
} allSatisfy: #yourself

"File I/O - Directory - Squeak, Gnu, Cuis"

'/home/rohan/sw/stsc3/help/' dirAllFilesWithExt: '.st'. "recursive directory listing by file extension"

"Evaluate - Squeak, Gnu, Cuis"

'1 + 1' evalString = 2. "=> true ; evaluate string"
'/home/rohan/sw/stsc3/help/graph/jmcc-why-supercollider.st' evalFile play "evaluate text file"

"Sc3"

Sc3 reset "C-c C-."
[ Sc3 nextId ] dup: 9

"Ui"

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
1 to: 5 do: [ :x | Transcript show: x; cr ]. "=> prints to Transcript"
Transcript << 'string' << ' ' << 'string' copy
(2 + 3) postln

"Method Finder"

ToolBuilder open: SelectorBrowser new. "open Method Finder (Selector Browser) tool ; squeak ; MVC"
SelectorBrowser new open. "squeak ; MVC"

"Message Names"

MessageNames openMessageNames. "open Message Names tool"

"Precedence"

{
(2 + 5 * 5) = 35.
(2 + (5 * 5)) = 27.
(2 + 5 min: 5) = 5.
(2 + (5 min: 5)) = 7.
(1 min: 2 max: 3) = 3.
((1 min: 2) max: 3) = 3.
(1 min: (2 max: 3)) = 1.
} allSatisfy: #yourself

"Boolean"

[ 0.5 coin ifTrue: [ $p ] ifFalse: [ $q ]  ] dup: 9 "=> p or q with equal probability"
1 coin = true "always true"
0 coin = false "always false"

"Object"

| x | x := 'x' dup: 2 sending: #copy. (x at: 1) ~~ (x at: 2). "duplicate by copying"
| x | x := 'x' dup: 2 sending: #yourself . (x at: 1) == (x at: 2). "duplicate without copying"
| x | x := 'x' dup: 2 sending: #value . (x at: 1) == (x at: 2). "duplicate by evaluating"
| x | x := 'x' dup: 2 . (x at: 1) ~~ (x at: 2). "at object duplicate by copying"
| x | x := 'x' dup. (x at: 1) ~~ (x at: 2) "duplicate by copying"

"Ordered Collection"

((OrderedCollection with: #x) detect: [ :each | each isSymbol ]) = #x.

"Random - Float"

5 randomFloat "=> uniform random Float in 0 to 5"
2 randomFloat: 3 "=> uniform random Float in 2 to 3"
0.2 rand + 0.1 "=> uniform random Float in 0.1 to 0.3"
4 rand2 "=> uniform random Float in -4 to 4"
1 expRand: 2 "=> exponential distribution"
[ 5 rand ] dup: 100"=> 100 random Floats in 0 to 5"
[ 0.2 coin ] dup: 100 "=> true 20%, false 80%"

"Random - Integer"

5 atRandom
5 randomInteger "=> random Integer in 1 to 5 ; i.e. atRandom"
1 randomInteger: 5 "=> random Integer in 1 to 5"
-4 randomInteger: 4 "=> random Integer in -4 to 4"
[ 5 randomInteger ] dup: 100 "=> 100 random Integers in 1 to 5"

"Random - Array"

#(1 2 3 4 5) atRandom "random element of Array ; i.e. atRandom ; choose"
#(1 2 3 4 5) choose "synonym for atRandom"
#(1 2 3 4 5) shuffled "=> i.e. copy shuffle"
[ #(1 2 3 4 5) atRandom ] dup: 100 "=> 100 randomly chosen numbers"
[ #('a' 'b' 'c' 'd' 'e') atRandom ] dup: 100 "=> 100 randomly chosen strings"

"Interval - Random"

(1 to: 9) atRandom
[ (1 to: 9) atRandom ] dup: 9 "=> array of 9 integers between 1 and 9"
[ (1 to: 9) choose ] dup: 9 "synonym for atRandom"

"Integer"

(0 to: 255) collect: [ :each | each asBinaryDigits: 8 ]
9 do: [ :each | each postln ]

"SequenceableCollection"

((1 to: 9) pyramid: 1) size = 45.
((' #SuperCollider ' asArray collect: #asciiValue) pyramid: 1) size = 136.
((' #SuperCollider ' asArray collect: #asciiValue) pyramid: 6) size = 256.

{
(#(1 2 3 4) pyramid: 1) = #(1 1 2 1 2 3 1 2 3 4).
(#(1 2 3 4) pyramid: 2) = #(4 3 4 2 3 4 1 2 3 4).
(#(1 2 3 4) pyramid: 3) = #(1 2 3 4 1 2 3 1 2 1).
(#(1 2 3 4) pyramid: 4) = #(1 2 3 4 2 3 4 3 4 4).
(#(1 2 3 4) pyramid: 5) = #(1 1 2 1 2 3 1 2 3 4 1 2 3 1 2 1).
(#(1 2 3 4) pyramid: 6) = #(4 3 4 2 3 4 1 2 3 4 2 3 4 3 4 4).
(#(1 2 3 4) pyramid: 7) = #(1 2 3 4 1 2 3 1 2 1 1 2 1 2 3 1 2 3 4).
(#(1 2 3 4) pyramid: 8) =  #(1 2 3 4 2 3 4 3 4 4 3 4 2 3 4 1 2 3 4).
(#(1 2 3 4) pyramid: 9) = #(1 1 2 1 2 3 1 2 3 4 2 3 4 3 4 4).
(#(1 2 3 4) pyramid: 10) = #(4 3 4 2 3 4 1 2 3 4 1 2 3 1 2 1).
(#(1 2 3 4) pyramidLinear: 1) = (#(1 2 3 4) pyramid: 1).
(#(1 2 3 4) pyramidLinear: 6) = (#(1 2 3 4) pyramid: 6).
} allSatisfy: #yourself

9 fib = (Array fib: 9).
#(1 2 3 4 5) sputter: 0.5 maxlen: 16

{
#(1 2 3) , #(4 5 6) = #(1 2 3 4 5 6).
(#(1) atWrap: 7) = 1.
(#(1 2 3 4) atWrap: 7) = 3.
#(#(1 2) #(3 4)) concatenation = #(1 2 3 4).
(#(5 4 3 2 1) withIndexCollect: [ :item :index | {item. index} ]) = #((5 1) (4 2) (3 3) (2 4) (1 5)).
[ #() extendCyclically: 5 ] on: Error do: [ true ].
(#(1) extendCyclically: 5) = #(1 1 1 1 1).
(#(1 2 3) extendCyclically: 5) = #(1 2 3 1 2).
((1 to: 3) extendCyclically: 5) = #(1 2 3 1 2).
#(1 (2 3) (4 5 6)) extendToBeOfEqualSize = #((1 1 1) (2 3 2) (4 5 6)).
{1 to: 9. 1 to: 3} extendToBeOfEqualSize = #((1 2 3 4 5 6 7 8 9) (1 2 3 1 2 3 1 2 3)).
(1 to: 9) extendToBeOfEqualSize = #((1) (2) (3) (4) (5) (6) (7) (8) (9)).
(3 * (1 to: 3)) extendToBeOfEqualSize = #((3) (6) (9)).
#((1 2) (3 4 5)) extendToBeOfEqualSize = #((1 2 1 ) (3 4 5)).
[ #(() (1)) extendToBeOfEqualSize ] on: Error do: [ true ].
(#(1 2 3) withExtending: #(4 5) collect: [ :x :y| x + y ]) = #(5 7 7). "1 + 4 and 2 + 5 and 3 + 4"
#(#(1 2) #(3 4)) transpose = #((1 3) (2 4)).
#(#(1 2) #(3 4)) transpose concatenation = #(1 3 2 4).
} allSatisfy: #yourself

[ #(1 2 3 4 5) shuffle ] on: Error do: [ true ]. "=> error, array is read only (Gnu)"
x := #(1 2 3 4 5) copy. x shuffle. x "=> random shuffle"
#(1 2 3 4 5) shuffled "=> i.e. copy shuffle"

#() atWrap: 7 "=> error"

#(5 4 3 2 1) withIndexDo: [ :item :index | Transcript show: {item. index}; cr ] "=> prints (5 1) (4 2) (3 3) (2 4) (1 5)"

#(1 3 5 7 9) integrate = #(1 4 9 16 25) "incremental sum"
#(1 4 9 16 25) differentiate = #(1 3 5 7 9) "pairwise difference"

"Array"

#(1 2 3 4 5) "literal array syntax ; {1. 2. 3. 4. 5}"
#(true false nil) "=> #(true false nil)"
#(#(1 2) #(3 4)) "literal array syntax ; {{1. 2}. {3. 4}}"

{
#(1 2 3 4 5) size = 5. "=> true"
(#(1 2 3) with: #(4 5 6) collect: [:i :j | {i. j}]) = #((1 4) (2 5) (3 6)).
(1 dup: 6) = #(1 1 1 1 1 1). "replicate by copying"
(0 to: 4) isInterval.
(0 to: 4 collect: [:x | x * x]) sum = 30.
(1 to: 5 collect: [:x | x * x]) = #(1 4 9 16 25).
(0 to: 4 collect: [:x | x * x]) = #(0 1 4 9 16).
(#(1 2 3 4 5) groupsOf: 2) = #((1 2 ) (3 4)).
(#(1 2 3 4 5 6) clump: 2) = #((1 2 ) (3 4) (5 6)).
#(1 2 3) ++ #(4 5) = #(1 2 3 4 5).
#(1 2 3) , #(4 5) = #(1 2 3 4 5).
(Array with: 1) = #(1).
(Array with: 1 with: 2) = #(1 2).
(Array with: 1 with: 2 with: 3) = #(1 2 3).
(Array with: 1 with: 2 with: 3 with: 4) = #(1 2 3 4).
(Array with: 1 with: 2 with: 3 with: 4 with: 5) = #(1 2 3 4 5).
(Array with: 1 with: 2.3 with: 'str') = #(1 2.3 'str').
(10 series: 0 value: 1/10) = {0. 1/10. 1/5. 3/10. 2/5. 1/2. 3/5. 7/10. 4/5. 9/10}. "Gnu..."
(5 geom: 1 value: 3) = #(1 3 9 27 81).
(#(1 2 3 4 5) nth: 3) = 3. "=> 3 ; Sc and St disagree about indexing, nth: is one-indexed at both"
({1. 2. 3} at: 2 put: -2) == -2. "=> true (at:put: returns the element put)"
(#(1 2 3 4 5) rotatedRight: 1) = #(5 1 2 3 4).
(#(1 2 3 4 5) rotatedLeft: 1) = #(2 3 4 5 1).
(#(1 2 3 4 5) rotated: 1) = #(5 1 2 3 4).
#(1 2 3 4) containsCollection = false.
#(1 (2 3) 4) containsCollection = true.
(#(1 3 5 7) indexOf: 5) = 3.
(#(1 3 5 7) indexOf: 9 ifAbsent: [ '?' ]) = '?'.
(#(1 3 5 7) detect: [ :item | item > 5]) = 7.
(#(1 3 5 7) detect: [ :item | item > 7] ifNone: [ '?' ]) = '?'.
(#(1 3 5 7) detect: [ :item | item > 5] ifFound: [ :item | item * item ] ifNone: [ '?' ]) = 49. "squeak; Gnu: dnu"
} allSatisfy: #yourself

Array fill: 5 value: [10 rand] "=> 5 random numbers between 1 and 10"

10 iota "=> 10 element array from 1 to 10"
10 iota - 1 * 0.1 "=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)"

10 from: 1 by: 1 "=> Interval equal to 10 element array from 1 to 10 by 1"
3 from: 3 by: 6 "=> Interval equal to 3 element array from 3 to 15 by 6"

"Array - Math"

{
(3 adaptToCollection: #(1 2 3) andSend: #*) = #(3 6 9).
#(1 2 3) * 3 = #(3 6 9).
3 * #(1 2 3) = #(3 6 9).
#(1 2 3) exp = #(2.718281828459045 7.38905609893065 20.085536923187668).
#(1 2) + #(3 4 5) = #(4 6 6). "requires modification of squeak, which would raise an error (collections not same size)"
#(1 2 3) + #(4 5) = #(5 7 7).
#(#(1 2) #(3 4)) allTuples = #((1 3) (1 4) (2 3) (2 4)).
#(#(1 2) #(3 4) #(4 5) #(6 7 8 9)) allTuples size = 32.
} allSatisfy: #yourself

| x | x := #(#(4 4.5) #(2 3 5 6)). (x crossedMultiply: x) = #((16 20.25) (8 13.5 20 27) (8 13.5 20 27) (4 9 25 36))
| x | x := #(#(4 4.5) #(2 3 5 6)). (x withCrossed: x collect: [ :i :j | i * j ]) = #((16 20.25) (8 13.5 20 27) (8 13.5 20 27) (4 9 25 36))
| x | x := #(#(4 4.5) #(2 3 5 6)). (x perform: #* crossedWith: x) = #((16 20.25) (8 13.5 20 27) (8 13.5 20 27) (4 9 25 36))

"Array - Expressions"

{'a' copy. {'b' copy. {'c' copy. {'d' copy. {'e' copy. {'f' copy. {'g' copy}}}}}}} = #('a' ('b' ('c' ('d' ('e' ('f' ('g'))))))).

"Interval"

{
(Interval from: 5 to: 9) asArray = #(5 6 7 8 9).
(Interval from: 5 to: 15 by: 2) asArray = #(5 7 9 11 13 15).
(1 to: 5 collect: [:x | x * x]) = #(1 4 9 16 25).
(1 to: 9 by: 2 collect: [:x | x * x]) = #(1 9 25 49 81).
} allSatisfy: #yourself

"Strings"

{
'file' , '.ext' = 'file.ext'.
3.141 printString = '3.141'.
(3.141 printStringLimitedTo: 4) = '3.14...etc...'. "Gnu: dnu"
Array class printString = 'Array class'. "stsc3-st: broken"
('aString' asArray collect: #asciiValue) = #(97 83 116 114 105 110 103).
} allSatisfy: #yourself

'\n' "=> '\n'"
'\\n' "=> '\\n'"
'\0' "=> '\0'"
'
x' "=> newline x"
| x | x := 'x'. {x == x. x == x copy. x = x. x = x copy}

'one {1} three' format: 'two'

"Symbols"

#x isSymbol.

"Haskell - List"

{
(1 to: 5) first = 1. "head"
(1 to: 5) last = 5.
(1 to: 5) size = 5. "length"
(1 to: 5 collect: [:x | x * x]) = #(1 4 9 16 25). "map"
(1 to: 5 select: [:x | x > 3]) = #(4 5). "filter"
((1 to: 5) copyFrom: 2 to: 5) = #(2 3 4 5). "drop"
((1 to: 5) at: 3) = 3. "haskell=! ; one-indexed"
} allSatisfy: #yourself

"Association"

(Association key: #x value: 1) = (#x -> 1).
(Association key: 0.1 value: 0.9) = (0.1 -> 0.9).

"Dictionary"

d := Dictionary new. d at: 'select' put: 'x' ; at: 'from' put: 'y' ; at: 'list' put: 'z'
d := Dictionary newFrom: {'select' -> 'x'. 'from' -> 'y'. 'list' -> 'z'}
d := Dictionary newFromPairs: {'select'. 'x'. 'from'. 'y'. 'list'. 'z'}

"Conditionals"

(nil isNil ifTrue: ['x'] ifFalse: ['y']) = 'x'.

"Collection"

{
#(1 2 3 4 5) anySatisfy: [ :item | item = 3 ].
#(1 2 3 4 5) sum = 15.
#(1 2 3 4 5) product == (1 * 2 * 3 * 4 * 5).
} allSatisfy: #yourself

"Class"

{
'x' class = String. "true"
'x' class class = String class. "true"
'x' class class class = Metaclass. "true"
'x' class class class class = Metaclass class. "true"
'x' class class class class class = Metaclass. "true"
} allSatisfy: #yourself

{
SinOsc name = #SinOsc. "true"
SinOsc class name = #'SinOsc class'. "Gnu: nil"
SinOsc new class name = #SinOsc. "true"
SinOsc class class name = #Metaclass. "true"
SinOsc class class class name = #'Metaclass class'. "Gnu: nil"
SinOsc class class class class name = #Metaclass. "true"
SinOsc superclass name = #ScUgen. "true"
SinOsc superclass superclass name = #Object. "true"
SinOsc subclasses size = 0. "true"
} allSatisfy: #yourself

ScUgen subclasses size = 236. "true"

SinOsc methodDict. "Squeak"
SinOsc methodDictionary. "Gnu Smalltalk"

SinOsc inspect.
SinOsc class inspect.
SinOsc new inspect.

True class superclass = True superclass class. "=> true"
Object superclass = nil.                       "=> true ; Gnu "
Object class superclass = Class.               "=> true ; Gnu"

Object superclass = ProtoObject               "=> true ; Squeak"
ProtoObject class superclass = Class          "=> true ; Squeak; Gnu = Object"

$c class = Character.                          "=> true"

(1 perform: #class withArguments: #()) = SmallInteger.
(1 perform: #class withArguments: #() inSuperclass: SmallInteger) = SmallInteger. "Gnu: dnu"
(1 perform: #class withArguments: #() inSuperclass: Integer) = SmallInteger. "Gnu: dnu"

"Time"

Time millisecondClockValue

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

"Process"

p := [ (Delay forSeconds: 3) wait. Transcript show: 'after delay'; cr ] fork. "Start a process that waits for three seconds then prints a message."
Transcript show: 'during delay'; cr.
p

p := [ 5 timesRepeat: [ (Delay forSeconds: 1) wait. Transcript show: 'after delay'; cr ] ] fork.
Transcript show: 'during delay'; cr.
p priority: Processor activePriority

Processor timeSlice = 40
Processor highestPriority = 9
Processor timingPriority = 8

[ 9 timesRepeat: [ (SinOsc freq: (Rand lo: 220 hi: 440) phase: 0 mul: 0.1) play. (3 / 2) seconds wait ]. Sc3 reset ] fork

[
	99 timesRepeat: [
		| o e s |
		o := SinOsc freq: (Rand lo: 220 hi: 440) phase: 0.
		e := Line start: 0.1 end: 0 dur: 7 doneAction: 2.
		s := Pan2 in: o pos: (Rand lo: -1 hi: 1) level: e.
		s draw.
		s play.
		2 seconds wait
	]
] fork.
100 seconds wait

t := Timer every: 1000 do: [ Transcript show: 'timer'; cr ] stopWhen: [ false ].
t stop

g := Generator on: [ :gen | 1 to: 23 do: [ :each |each even ifTrue: [ gen yield: each ] ] ]
g next

"stsc3-fork-region (emacs)"
99 timesRepeat: [
	(Pan2 apply: {(SinOsc apply: {(Rand apply: {220. 440}). 0}). (Rand apply: {-1. 1}). (Line apply: {0.1. 0. 7. 2})}) play .
	2 seconds wait
]

"Sensor"

Sensor class = EventSensor
Sensor anyButtonPressed
Sensor blueButtonPressed "Third (right) button"
Sensor commandKeyPressed
Sensor controlKeyPressed
Sensor cursorPoint
Sensor mouseButtons
Sensor redButtonPressed "First (left) button"
Sensor shiftPressed
Sensor waitButton "Return location of button press"
Sensor waitClickButton "Return location of button release"
Sensor yellowButtonPressed "Second (middle) button"

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

d := {'a'->1. 'b'->2. 'c'->'3'} as: Dictionary. "Gnu: dnu"
FileStream forceNewFileNamed: '/tmp/dict.st' do: [:out | d storeOn: out]. "store"
FileStream oldFileNamed: '/tmp/dict.st' do:  [:in | Object readFrom: in]. "read"
FileStream stdout nextPutAll: 'Hello, World'; cr; flush. "write to stdout"

"Stream"

{
(String streamContents: [ :aStream | 'hello there!\n' printOn: aStream ]) = '''hello there!'''.
(ByteArray streamContents: [ :aStream | aStream int32: ScGraph scgf ]) asArray = #(83 67 103 102).
(ByteArray streamContents: [ :aStream | aStream float32: 3.141 ]) asArray = #(64 73 6 37).
} allSatisfy: #yourself

"Smalltalk"

(Smalltalk at: #Array) = Array. "true"
(Smalltalk at: #SinOsc) = SinOsc. "true"

Smalltalk garbageCollect. "Squeak"
Smalltalk aboutThisSystem. "System Reporter"

"Math"

{
69 midiCps = 440.
440 cpsMidi = 69.
-32 dbAmp closeTo: 0.025118864315095784.
-32 dbAmp ampDb rounded = -32.
(0 raisedTo: 1) = 0.
(1 raisedTo: 0) = 1.
(0.0 raisedTo: 1.0) = 0.0.
(1.0 raisedTo: 0.0) = 1.0.
(0.0 raisedTo: 0.0) = 1.0.
(1.0 / 0.0) = Float infinity.
(0.0 / 0.0) isNaN.
Float pi = 3.141592653589793.
Float pi * 2 = 6.283185307179586.
([ :x | x * x ] value: Float pi * 2) = 39.47841760435743.
(0.5 linExpFrom: 1 to: 2) = 1.414213562373095.
(0.5 linExpFrom: 1 to: 2) = 1.414213562373095.
13 \\ 12 = 1. "13 modulo 12"
13 \\ 12.5 = 0.5. "13 modulo 12.5"
13 % 12 = 1.
13 % 12.5 = 0.5.
((SinOsc freq: 440 phase: 0 mul: 13) % 12) isUgen.
(13 % (SinOsc freq: 440 phase: 0 mul: 12)) isUgen.
} allSatisfy: #yourself

(-1 raisedTo: (1 / 3)) isNaN. "=> -1 (Squeak) ; Gnu: error"
(-1 raisedTo: 0.5) isNaN. "=> ArithmeticError: Negative can't be raised to float powers."
(-1 raisedTo: (1 / 2)) isNaN. "Gnu: ArithmeticError: Negative numbers don't have even roots. c.f. nthRoot:"
Float infinity printString = 'Infinity'. "gst Inf"
(0 raisedTo: 0) = 1. "gst = error"
(0 to: 10) collect: [ :n | (n / 10) linExpFrom: 1 to: 2 ] "=> 10 point equally spaced points on exponential curve from 1 to 2"

1 / 0 "=> error"

"BinaryOpUgen"

(BinaryOpUgen selector: #* a: 0.1 b: (SinOsc freq: 440 phase: 0)) draw
(BinaryOpUgen selector: #% a: 0.1 b: (SinOsc freq: 440 phase: 0)) draw

"Gnu Smalltalk"

Smalltalk system: 'ls /home/rohan/sw/stsc3/README.md'.
Smalltalk getenv: 'HOME'. "/home/rohan"

ObjectMemory snapshot. "save image file (to current image file)"
ObjectMemory snapshot: '/tmp/stsc3.img'. "save image file (to indicated image file)"
ObjectMemory compact. "garbage collection (major)"
ObjectMemory scavenge. "garbage collection (minor)"
ObjectMemory current inspect. "vm memory statistics"
File image asString. "location of image file"

"gst-load Sockets"
PackageLoader fileInPackage: 'Sockets'.
FileStream fileIn: '/home/rohan/sw/stsc3/st/Sc3-Filein-For-Gnu.st'.

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

"Block"

| factorial |
factorial := [:n| n = 1 ifTrue: [1] ifFalse: [(factorial value: n - 1) * n]].
((1 to: 10) collect: factorial) = #(1 2 6 24 120 720 5040 40320 362880 3628800).
[ 10 rand ] dup: 10 "=> at block duplicate by evaluating, ten random numbers between 1 and 10"
[ 10 rand ] dup "=> two random numbers between 1 and 10"

{
[] class = BlockClosure.
([ :x | x := 2. x] value: 1) = 2. "Gnu Smalltalk disallows assignment to arguments"
([ 1 ] cull: 2) == 1.
([ :x | x ] cull: 2) == 2.
([ :x :y | x * y ] valueWithArguments: #(3 4)) == 12.
([ :x :y :z | x * y + z ] apply: #(3 4 5)) == 17.
([:w :x :y :z | 'hello' ] value: 1 value: 2 value: 3 value: 4) = 'hello'.
([:v :w :x :y :z | 'hello' ] value: 1 value: 2 value: 3 value: 4 value: 5) = 'hello'.
} allSatisfy: #yourself

"Integer"

{
5 iota asArray = #(1 2 3 4 5).
(5 from: 3 by: 4) asArray = #(3 7 11 15 19). "Myself number of places from start by step"
(5 from: 2 by: 0.5) asArray = {2. 5/2. 3. 7/2. 4}.
(1 to: 9 collect: [ :item | item * item ]) = #(1 4 9 16 25 36 49 64 81).
} allSatisfy: #yourself

"Number"

(-20 to: 30) collect: [ :each | each wrap: 0 hi: 10 ]
(-20 to: 20) collect: [ :each | each wrap: -10 hi: 10 ]
8 fibonacciSequence asArray = #(1 1 2 3 5 8 13 21)

"Delay"

(Delay forMilliseconds: 500) wait

"Open Sound Control"

m := OscMessage at: '/n_set' with: #(-1 'freq' 440 'amp' 0.1).
e := m encode asArray.
e size = 40.
e = #(47 110 95 115 101 116 0 0 44 105 115 105 115 102 0 0 255 255 255 255 102 114 101 113 0 0 0 0 0 0 1 184 97 109 112 0 61 204 204 205)
m asString = '/n_set ,isisf -1 freq 440 amp 0.1 '
b := OscBundle at: 0 with: {m}
b asString = '#bundle 0 1 /n_set ,isisf -1 freq 440 amp 0.1 '
e := b encode asArray.
e size == 60
e = #(35 98 117 110 100 108 101 0 0 0 0 0 0 0 0 0 0 0 0 40 47 110 95 115 101 116 0 0 44 105 115 105 115 102 0 0 255 255 255 255 102 114 101 113 0 0 0 0 0 0 1 184 97 109 112 0 61 204 204 205 )
{b} writeNrt: '/tmp/nrt.osc'

| new load |
new := OscMessage at: '/s_new' with: #('tmp' -1 0 1).
load := OscMessage at: '/d_load' with: { '/tmp/tmp.scsyndef'. new encode}.
load encode size = 68

{
m address = '/n_set'.
m arguments = #(-1 'freq' 440 'amp' 0.1).
(m arguments collect: #oscTypeTag) = #($i $s $i $s $f).
m descriptor = ',isisf'.
m asString = '/n_set ,isisf -1 freq 440 amp 0.1 '.
} allSatisfy: #yourself

m := OscMessage at: '/n_set' with: #(-1 'freq' 440 'amp' 0.1)
m printOscOn: Transcript
(m encode) sendUdpToHost: '127.0.0.1' port: 57110

m := OscMessage at: '/memset' with: #('addr' (0 1 15 16 255)).

{
m descriptor = ',sb'.
m asString = '/memset ,sb addr 00010F10FF'.
} allSatisfy: #yourself

m printOscOn: Transcript

s := WriteStream on: (ByteArray new: 20)
1234 putOscOn: s "must be in network byte order"
3.14 putOscOn: s "must be in network byte order"
'rq' putOscOn: s "must be 4-byte aligned and nul terminated"
#(1 2 3) putOscOn: s "must be length prefixed in network order and 4-byte aligned"
s contents asArray = #(0 0 4 210 64 72 245 195 114 113 0 0 0 0 0 3 1 2 3 0 )

"Testing"

TestRunner open "squeak"

"Parsing"

{
(FortranNumberParser parse: 'NaN') isNaN.
(FortranNumberParser parse: 'Infinity') isInfinite.
(FortranNumberParser parse: '-Infinity')  isInfinite.
(FortranNumberParser parse: '100') = 100.
(FortranNumberParser parse: '-99') = -99.
(FortranNumberParser parse: '3.14') = 3.14.
(FortranNumberParser parse: '-1.4') = -1.4.
(FortranNumberParser parse: '1e6') = 1e6.
(FortranNumberParser parse: '-1e6') = -1e6.
(FortranNumberParser parse: '1e-1') = 1e-1.
(FortranNumberParser parse: '-1e-1') = -1e-1.
} allSatisfy: #yourself

"Set"

s := Set new
c := Color black
s add: c
(s scanFor: c) notNil.
(s array at: (s scanFor: c)) = c.
(s like: c) = c.
s includes: c.

"LocalBuf ; BufAlloc"

#(0 2 4 5 7 9 10 12) asLocalBuf draw

"Socket - Squeak"

| s m |
s := Socket udpCreateIfFail: [ self error: 'failed to create Udp socket' ].
m := (OscMessage at: '/g_freeAll' with: #(1)) encodeOsc.
s sendUDPData: m toHost: '127.0.0.1' port: 57110.

"Socket - Gnu"

| s m |
s := Sockets.DatagramSocket remote: '127.0.0.1' port: 57110 local: nil port: 0.
m := (OscMessage at: '/g_freeAll' with: #(1)) encodeOsc.
s nextPut: (Sockets.Datagram data: m); flush.
s close.

"Float"

3.141 asIEEE32BitWord = 1078527525.
9.0 do: [ :each | each postln ]

"Tracing"

MessageTally tallySends: [3.14159 printString] "Squeak"

"Ston"

{
(STON toString: (3.141 @ 0)) = 'Point[3.141,0]'. "Point{#x:3.141,#y:0}"
(STON toString: (#key -> 'value')) =  '#key:''value'''.
(STON toString: {1 to: 10 by: 2. 3 days}) =  '[Interval{#start:1,#stop:10,#step:2},Duration{#nanos:0,#seconds:259200}]'.
(STON toString: {1. 3.141. 2 / 5}) = '[1,3.141,2/5]'.
(STON toString: {$x. 'y'. #z}) = '[Character[''x''],''y'',#z]'.
(STON toString: (ByteArray newFrom: #(0 1 255))) = 'ByteArray[''0001ff'']'.
(STON toString: true) = 'true'.
(STON toString: #(true false)) =  '[true,false]'.
(STON toString: (Dictionary newFrom: {#true -> true. #false -> false})) = '{#true:true,#false:false}'.
} allSatisfy: #yourself

{
(STON fromString: '1') = 1.
(STON fromString: '[3.141]') = {3.141}.
(STON fromString: '#x') = #x.
(STON fromString: '[3.141,0]') = #(3.141 0).
(STON fromString: '[true,false]') = #(true false).
(STON fromString:'{#true:true,#false:false}') = (Dictionary newFrom: {#true -> true. #false -> false}).
(STON fromString: 'ByteArray[''0001ff'']') = (ByteArray newFrom: #(0 1 255)). "class must be loaded"
(STON fromString: 'Character[''x'']') = $x.
} allSatisfy: #yourself

"MappedCollection"

d := Dictionary new.
d at: 'dog' put: 'hundo'; at: 'cat' put: 'kato'; at: 'snake' put: 'serpento'.
m := MappedCollection collection: d map: d keys asArray sorted.
((1 to: m size) collect: [ :index | m at: index]) = #('kato' 'hundo' 'serpento')

a := Array withAll: (1 to: 100).
i := (1 to: 100) select: [ :index | (a at: index) even ].
m := MappedCollection collection: a map: i.
((1 to: 10) collect: [ :index | m at: index ]) =  #(2 4 6 8 10 12 14 16 18 20)

"Emacs"
(setq stsc3-interpreter (list "stsc3-st"))
(setq stsc3-interpreter (list "gst"))
