'A Terse Guide to Smalltalk - c.f. http://wiki.squeak.org/squeak/5699 by Chris Rathman'

'Arithmetic expressions'

{
6 + 3 = 9.                                                   "addition"
6 - 3 = 3.                                                   "subtraction"
6 * 3 = 18.                                                  "multiplication"
1 + 2 * 3 = 9.                                               "evaluation always left to right (1 + 2) * 3"
6 / 3 = 2.                                    "integer division with integer result"
(5 / 3) class = Fraction.                                    "integer division with non-integer result"
((5 / 3) asFloat) closeTo: 1.666666.                         "fraction as float"
5.0 / 2.0 = 2.5.                                             "division with float result (operand float)"
(355.0 / 113) closeTo: 3.141592.                             "division with float result (operand integer)"
9 // 12 = 0.                                                 "integer divide"
9 \\ 12 = 9.                                                 "integer remainder (modulo)"
5.0 // 3.0 = 1.                                              "integer divide"
5.0 \\ 3.0 = 2.                                              "integer remainder"
-5 = 5 negated.                                              "unary minus"
5 sign = 1.                                                  "numeric sign (1, -1 or 0)"
5 negated = -5.                                              "negate receiver"
1.25 integerPart = 1.0.                                      "integer part of number"
1.25 fractionPart = 0.25.                                    "fractional part of number"
5 reciprocal = (1/5).                                        "reciprocal function"
6 * 3.1 = 18.6.                                              "auto convert to float"
5 squared = 25.                                              "square function"
25 sqrt = 5.0.                                               "square root"
(5 raisedTo: 2) = 25.                                        "power function"
(5 raisedToInteger: 2) = 25.                                 "power function with integer"
1 exp = Float e.                                             "exponential"
-5 abs = 5.                                                  "absolute value"
3.99 rounded = 4.                                            "round"
3.99 truncated = 3.                                          "truncate"
(3.99 roundTo: 1) = 4.0.                                     "round to specified decimal places"
(3.99 truncateTo: 1) = 3.0.                                  "truncate to specified decimal places"
3.99 floor = 3.                                              "round down"
3.99 ceiling = 4.                                            "round up"
5 factorial = 120.                                           "factorial"
(-5 quo: 3) = -1.                                            "integer divide rounded toward zero"
(-5 rem: 3) = -2.                                            "integer remainder rounded toward zero"
(28 gcd: 12) = 4.                                            "greatest common denominator"
(28 lcm: 12) = 84.                                           "least common multiple"
Float e ln = 1.0.                                            "natural logarithm"
1.0 exp ln = 1.0.                                            "natural logarithm"
100 log = 2.0.                                               "base 10 logarithm"
(10.0 raisedTo: 2.0) log = 2.0.                              "base 10 logarithm"
(100 log: 10) = 2.0.                                         "logarithm with specified base"
(100 floorLog: 10) = 2.                                      "floor of the log"
Float radiansPerDegree = (Float pi / 180).                   "radians per degree"
180 degreesToRadians = Float pi.                             "convert degrees to radians"
Float pi radiansToDegrees = 180.0.                           "convert radians to degrees"
(Float pi / 2) sin = 1.0.                                    "sine"
0.0 cos = 1.0.                                               "cosine"
0.0 tan = 0.0.                                               "tangent"
(10 max: 20) = 20.                                           "get maximum of two numbers"
(10 min: 20) = 10.                                           "get minimum of two numbers"
Float pi = 3.141592653589793.                                "pi"
Float e = 2.718281828459045.                                 "exp constant"
100 atRandom between: 1 and: 100.                            "random integer in (1,self)"
1.0 atRandom between: 0 and: 1.0.                            "random float in (0,self)"
0 isZero.
1 isZero not.
} allSatisfy: #yourself

'ArrayedCollection'

{
#(3 2 1) copy sort = #(1 2 3). "merge sort"
(9 to: 1 by: -1) asArray sort = (1 to: 9) asArray. "merge sort"
(1 to: 99) asArray shuffle sort = (1 to: 99) asArray. "merge sort"
(1 to: 99) asArray shuffle quickSort = (1 to: 99) asArray.
'string' copy quickSort = 'ginrst'.
'quick' copy quickSort = 'cikqu'.
'merge' copy sort = 'eegmr'. "merge sort"
'mergeSortFrom:to:by:' copy sort = ':::FSbeegmmooorrrtty'. "merge sort"
'quickSort' copy quickSort = 'Scikoqrtu'.
(#(5 4 3 2 1) copy sort: [ :lhs :rhs | lhs < rhs ]) = #(1 2 3 4 5).
(#(5 4 3 2 1) copy sort: #<) = #(1 2 3 4 5).
} allSatisfy: #yourself

letters := 'abcdefghijklmnopqrstuvwxzy' copy.
{
letters quickSort = letters.
letters sort = letters. "merge sort"
letters reverse quickSort = letters.
letters reverse sort = letters. "merge sort"
letters reverse asArray sort = letters asArray. " merge sort"
letters shuffle sort = letters.
} allSatisfy: #yourself

'Array'

{
Array isVariable = true.                                     "array is indexable"
(Array withAll: #(1 2 3 4)) = #(1 2 3 4).                    "array from collection"
(#(4 5 6 7) atAll: #(2 3)) = #(5 6).                         "at: for each index of array of indices"
(#(1 2 3 4 5) copyFrom: 2 to: 4) = #(2 3 4).                 "copy range"
#(1 (2 (3 4))) = #(1 #(2 #(3 4))).                           "in st-80 sub-arrays do not require # prefix"
#(x x: nil) = #(#x #x: nil).                                 "in st-80 symbols do not require #prefix"
(#(5 4 3 2 1) detect: [ :i | i < 3 ]) = 2.                   "detect element (not index), so 2"
(#(1 2 3 4 5) take: 2) = #(1 2).
(#(1 2) take: 5) = #(1 2).
(#(1 2) readStream take: 5) = #(1 2).
#(1 2) == #(1 2).                                            "literals are identical"
#(1 2) ~~ #(1 2) shallowCopy.                                "copies are not identical"
{1. 2} ~~ {1. 2}.                                            "array expressions are not identical"
{1. 2} ~~ {1. 2} shallowCopy.                                "also when copied"
({1. 2. 3} at: 1 put: '1') = '1'.                            "array expressions are mutable"
#(1 2 3) == #(1 2 3).                                        "array literals are identical"
#(1 2 3) ~~ {1. 2. 3}.                                       "array literals and array expressions are not identical"
[ Array new: -1 ] on: Error do: [ true ].                         "error"
(#(1 2 3 4 5) any: 3) = #(1 2 3).                            "any three are first three"
((Array new: 2) fillFrom: #(3 4) with: [ :n | n * n ]) = #(9 16).
((Array new: 2) fillFrom: #(9 16) with: #sqrt) = #(3 4).
} allSatisfy: #yourself

c := {'string'. #symbol. 42}.
{
(c at: 1) = 'string'.
(c at: 2) = #symbol.
(c at: 3) = 42.
} allSatisfy: #yourself

3 timesRepeat: [ Transcript show: 'repeat'; cr ]            "prints repeat three times, returns count"
#(1 2 3) do: [ :item | Transcript show: item; cr ]              "prints 123"
x := {1. 2. 3}. {(x at: 2 put: $2) = $2. x = {1. $2. 3}}.    "mutate slot, returns the value put"
#(1 2.3 '4' $5 #'6' #seven #(8 9) nil)                      "array literal syntax"
#(1 true 3 #four) printString
#(1 2 3) at: 1 put: '1'                                     "error: array literals are immutable (gnu)"

'Assignment'

x := 5                                                      "assignment"
x := y := z := 6                                            "compound assignment"
x := (y := 6) + 1                                           "assignment value, y = 6, x = 7"
x := Object new                                             "bind to allocated instance of a class"
x := 123 class                                              "discover the object class"
x := Integer superclass                                     "discover the superclass of a class"
x := Object allInstances                                    "get an array of all instances of a class"
x := Integer allSuperclasses                                "get all superclasses of a class"
x := 1.2 hash                                               "hash value for object"
y := x copy                                                 "copy object"
y := x shallowCopy                                          "copy object (not overridden)"
y := x deepCopy                                             "copy object and instance vars"
x := 3. ('string' at: (x := x + 1)) = $i

'Association'

Association key: #x value: 1                                "associate key with value"
#x -> 1 = (Association key: #x value: 1)                    "arrow message at Object"
(#x -> 1) key = #x                                          "key (left hand side) of association"
(#x -> 1) value = 1                                         "value (right hand side) of association"
(#x -> 1) copy
{#x -> 1. #y -> 2} as: Dictionary
(#x -> 1) printString
(#+ -> 1) printString

'Bag'

Bag isVariable not
Bag withAll: #(1 2 2 3 3 3)
Bag withAll: #(x y y z)
Bag newFrom: {#x -> 2 . #y -> 3 . #y -> 4. #z -> 2 }
{#x -> 2 . #y -> 3 . #y -> 4. #z -> 2 } as: Bag
#(1 2 2) asBag = #(1 2 2) asBag
#(x y z) asBag = ({'x'. 'y'. 'z'} collect: [ :each | each asSymbol ]) asBag
b := Bag new add: 'a' withOccurrences: 4; add:'b' withOccurrences: 2; yourself
b = b copy                      "copies are equal"

'Behaviour'

{
Float selectors size = 39.                                   "array of selectors implemented at class"
Float allSelectors size = 242.                               "get all selectors an object responds to"
Double includesSelector: #+.                                 "test if class implements selector"
(Float includesSelector: #+) not.                            "does not look in superclasses"
(Float canUnderstand: #+).                                   "test if class can understand selector"
Float superclass = Number.                                   "superclass of class"
Float inheritsFrom: Number.                                  "immediate inheritance"
Float inheritsFrom: Magnitude.                               "non-immediate inheritance"
} allSatisfy: #yourself

Float selectorsDo: [ :item | Transcript show: item; cr ]    "perform block at each selector"
Float withAllSuperclassesDo: [:c | Transcript show: c; cr]  "perform block at self and each superclass in turn"

'Bitwise Manipulation'

{
(255 bitAnd: 15) = 15.                                       "and bits"
(240 bitOr: 15) = 255.                                       "or bits"
(255 bitXor: 240) = 15.                                      "xor bits"
255 bitInvert = -256.                                        "invert bits"
(15 bitShift: 4) = 240.                                      "left shift"
(240 bitShift: -4) = 15.                                     "right shift"
(128 bitAt: 7) = 0.                                          "bit at position (0|1) [!Squeak]"
(255 allMask: 15).                                           "test if all bits set in mask set in receiver"
(255 anyMask: 15).                                           "test if any bits set in mask set in receiver"
(255 noMask: 15) not.                                        "test if all bits set in mask clear in receiver"
} allSatisfy: #yourself

'BlockClosure'

{
[ 42 ] class = BlockClosure.
([ :aNumber | aNumber * aNumber ] value: 3) = 9.             "apply block to one argument"
([ :aNumber | aNumber * aNumber ] cull: 3) = 9.              "apply block with or without argument"
([ 42 ] cull: 3) = 42.                                       "apply block with or without argument"
[ [ 42 ] value: nil ] on: Error do: [ true ]. "error"
[ [ 42 ] value: nil ] ifError: [ true ]. "error"
[ [ :item | 42 ] value ] on: Error do: [ true ]. "error"
[ [ :item | 42 ] value ] ifError: [ true ]. "error"
([ :item | item := nil ] value: 42) isNil. "arguments are mutable (not in gst)"
[ 42 ] ~= 42.
} allSatisfy: #yourself


b := [ :x | x * 42 ].
{
b = b copy.
(b copy value: 2) = 84.
b = ({ b } copy at: 1).
} allSatisfy: #yourself

cnt := 1. [ cnt > 9 ] whileFalse: [ Transcript show: cnt; cr. cnt := cnt + 1 ]
cnt := 1. [ cnt <= 9 ] whileTrue: [ Transcript show: cnt; cr. cnt := cnt + 1 ]
cnt := 1. [ Transcript show: cnt; cr. cnt := cnt + 1. cnt > 9 ] whileFalse
cnt := 1. [ Transcript show: cnt; cr. cnt := cnt + 1. cnt <= 9 ] whileTrue
[ 1 unimplementedMessage ] on: Error do: [ :err | err printString ]
[ UnknownGlobal ] on: Error do: [ :err | 'Unknown global?' ]
[ nil error ] on: Error do: nil. "error (illegal block)"

[ :a | | a | a ] value: 1 "error - duplicate name error"

'Booleans'

{
1 = 1 = true.                                                "equality predicate (operator)"
1 ~= 2 = true.                                               "equality predicate (operator)"
(1 == 1).                                                    "identical"
1 ~~ 2.                                                      "not identical"
2 > 1.                                                       "greater than"
1 < 2.                                                       "less than"
1 >= 1.                                                      "greater than or equal"
1 <= 1.                                                      "less than or equal"
false not.                                                   "boolean not"
true & false = false.                                        "logical and (operator)"
true | false = true.                                         "logical or (operator)"
(true and: false) = false.                                   "logical and"
(true or: false) = true.                                     "logical or"
(false and: ['and' error]) = false.                          "logical and (short-circuit)"
(true or: ['or' error]) = true.                              "logical or"
true xor: false.                                             "test if one true and other false"
5 between: 3 and: 12.                                        "between (inclusive)"
123 isKindOf: Number.                                        "test if object is class or subclass of"
123 isMemberOf: SmallInteger.                                "test if object is type of class"
123 respondsTo: #sqrt.                                       "test if object responds to message"
true = true.                                                 "constant"
false = false.                                               "constant"
true not not = true.                                         "not"
nil isNil = true.                                            "test if object is nil"
1 positive = true.                                           "test if number is positive"
0 strictlyPositive = false.                                  "test if number is greater than zero"
-1 negative = true.                                          "test if number is negative"
2 even = true.                                               "test if number is even"
1 odd = true.                                                "test if number is odd"
1 isInteger = true.                                          "test if object is integer"
1.0 isFloat = true.                                          "test if object is float"
1.0 isNumber = true.                                         "test if object is number"
$A isUppercase = true.                                       "test if upper case character"
$a isLowercase = true.                                       "test if lower case character"
false asBit = 0.
true asBit = 1.
true asInteger > false asInteger.
} allSatisfy: #yourself
'ByteArray'

{
(ByteArray new: 4) hex = '00000000'.
(ByteArray new: 4) size = 4.
(ByteArray new: 4) byteSize = 4.
((ByteArray new: 4) at: 1) = 0.
((ByteArray new: 4) byteAt: 2) = 0.
((ByteArray new: 4) at: 1 put: 15; at: 3 put: 240; hex) = '0f00f000'.
((ByteArray new: 4) byteAt: 2 put: 15; byteAt: 4 put: 240; hex) = '000f00f0'.
(ByteArray newFrom: #(1 2 3 4)) hex = '01020304'.
((ByteArray new: 1) readHexFrom: '01' readStream) hex = '01'.
(ByteArray readHexFrom: '0f00f000') hex = '0f00f000'.
'string' asByteArray hex = '737472696e67'.
(ByteArray readHexFrom: '737472696e67') asString = 'string'.
((ByteArray new: 4) atAllPut: 15) hex = '0f0f0f0f'.
((ByteArray new: 4) word32At: 1 put: 123456) asArray = #(0 1 226 64).
} allSatisfy: #yourself

'Cascade messages'

Transcript show: 'hello '; show: 'world'; cr                "cascading, send multiple messages to receiver"
3 + 2; * 100                                                "=> 300, sends message to receiver, i.e to 3 not 5"
Set new add: 1; add: 2; yourself

'Character'

{
$ = Character space.
$	 = Character tab.
Character newLine = Character nl.
Character newPage asInteger = 12.
(Character value: 97) = $a.
$a asInteger = 97.
$A asInteger = 65.
$Z asInteger = 90.
$a = $a.
$a ~= $b.
$1 digitValue = 1.
$c isCharacter = true.
$a isLetter = true.
$1 isDigit = true.
$a isLowercase = true.
Character space isSeparator = true.
$e isVowel.
$a asUppercase = $A.
$A asLowercase = $a.
$a hash = $a asInteger hash.
$s ~= nil.
($a to: $z) = 'abcdefghijklmnopqrstuvwxyz'.
('/', Character separators) size = (Character separators size + 1).
('/', CharacterSet separators) size = (CharacterSet separators asArray size + 1).
$a printString = '$a'.
$5 printString = '$5'.
$@ printString = '$@'.
Character cr printString = 'Character cr'.
Character lf printString = 'Character lf'.
Character space printString = 'Character space'.
Character null printString = 'Character null'.
(Character value: 17) printString = 'Character value: 17'.
${ = ${.
$\ = $\.
$/ = $/.
${ asciiValue = 123.
$} asciiValue = 125.
} allSatisfy: #yourself

'CharacterSet'

{
CharacterSet ascii size = 127.                               "ascii character set"
CharacterSet crlf size = 2.                                  "carriage return and line feed"
CharacterSet separators size = 5.                            "whitespace"
(CharacterSet separators any: 1 as: Array) = {Character tab}.
} allSatisfy: #yourself

'Class'

Class superclass = ClassDescription.

'Collection'

{
#(1 3 2 9 4 5 7 8 6) max = 9.                                "maximum item"
#(1 3 2 9 4 5 7 8 6) includes: 6.                            "includes element"
#(1 3 2 9 4 5 7 8 6) includesAnyOf: #(0 6).                  "includes any element of a collection"
(#(1 2 2 3 3 3) histogramOf: [ :each | each]) = #(1 2 2 3 3 3) asBag.
(#(2 3 4 5 5 6) copyWithout: 5) = #(2 3 4 6).
[ #(1 2) take: -1 ] on: Error do: [ true ].
} allSatisfy: #yourself

'Color'

{
(Color brown * #(1 0 1)) printString = '(Color r: 0.6 g: 0 b: 0)'.
(Color blue + Color green) printString = '(Color r: 0 g: 1 b: 1)'.
(Color white - Color red) printString =  '(Color r: 0 g: 1 b: 1)'.
(Color red / 2) printString =  '(Color r: 0.5 g: 0 b: 0)'.
(Color lightBlue) printString =  '(Color r: 0.8 g: 1 b: 1)'.
(Color red: 0.2 green: 0.6 blue: 1.0) = (Color r: 0.2 g: 0.6 b: 1.0).
} allSatisfy: #yourself

(Color red lightShades: 12) do: [ :each | Transcript show: each; cr ]

'Compiler'

{
(Compiler evaluate: '3 + 4') = 7.
(Compiler evaluate: 'Compiler evaluate: ''3 + 4''') = 7.
(Compiler evaluate: 'Transcript show: 42; cr') = Transcript. "prints 42 on transcript"
} allSatisfy: #yourself

'Constants'

true                                                        "true constant"
false                                                       "false constant"
nil                                                         "nil object constant"
1                                                           "integer constants"
3.14                                                        "float constants"
2e-2                                                        "fractional constants"
16r0F                                                       "hex constant"
-1                                                          "negative constants"
'Hello'                                                     "string constant"
'I''m here'                                                 "single quote escape"
$A                                                          "character constant"
$                                                           "character constant (space)"
#aSymbol                                                    "symbol constants"
#(3 2 1)                                                    "array constants"
#('abc' 2 $a)                                               "mixing of types allowed"

'Context'

thisContext class
[ thisContext printString ] value
[ thisContext sender printString ] value
1 to: 3 do: [ :item | Transcript show: item; cr. item haltIf: [ item > 3] ] "halt if item is greater than three"

'Conversion'

{
3.99 asInteger = 3.                                          "convert number to integer (truncates in Squeak)"
3 asFloat = 3.0.                                             "convert number to float"
3 asFloat = 3.0.                                             "3 asFloat is equal to 3"
65 asCharacter = $A.                                         "convert integer to character"
$A asciiValue = 65.                                          "convert character to integer"
3.99 printString = '3.99'.                                   "convert object to string via printOn:"
3.99 storeString = '3.99'.                                   "convert object to string via storeOn:"
((2 raisedTo: 4) - 1 radix: 16) = 'F'.                       "convert to string in given base"
((2 raisedTo: 8) - 1 radix: 16) = 'FF'.
((2 raisedTo: 12) - 1 radix: 16) = 'FFF'.
((2 raisedTo: 16) - 1 radix: 16) = 'FFFF'.
(15 numberOfDigitsInBase: 16) = 1.
(16 numberOfDigitsInBase: 16) = 2.
} allSatisfy: #yourself

3.99 asFraction                                             "convert number to fraction (399/100)"

'Date'

{
(Date readFrom: '1986-05-23' readStream) printString = '23 May 1986'.
(Date readFrom: '01-23-2004' readStream) yyyymmdd = '2004-01-23'.
(Date readFrom: '1-01-2005' readStream) printString = '1 January 2005'.
} allSatisfy: #yourself

Date dateAndTimeNow first printString "30 May 2022"
Date dateAndTimeNow second printString "10:49:44 am"
Date today printString "30 May 2022"
(Date today previous: #'Thursday') yyyymmdd "2022-05-26"

'DateAndTime'

{
DateAndTime unixEpoch asSeconds = 2177452800.
DateAndTime epoch printString = '1901-01-01T00:00:00+00:00'.
DateAndTime unixEpoch printString = '1970-01-01T00:00:00+00:00'.
(DateAndTime unixEpoch - (Duration days: Time daysFromSmalltalkEpochToPosixEpoch)) = DateAndTime epoch.
(DateAndTime year: 1970 day: 1) printString = '1970-01-01T00:00:00+10:00'.
(DateAndTime year: 2003 month: 03 day: 22 hour: 12 minute: 0) makeUTC printString = '2003-03-22T12:00:00+00:00'.
(TimeStamp readFrom: '1-01-2005 0:00 am' readStream) printString = '1 January 2005 12:00 am'.
(DateAndTime year: 2005 month: 1 day: 1) dayOfWeek = 7.
} allSatisfy: #yourself

DateAndTime now printString "2022-05-30T01:44:29.492936+10:00"
DateAndTime localTimeZone printString "a TimeZone(UTC)"
t := DateAndTime year: 2000 month: 1 day: 3 hour: 4 minute: 5 second: 6 nanoSecond: 7 offset: 8 hours
t printString
t julianDayNumber = 2451547
t offset = 6 hours
t hour = 2
t minute = 3
t second = 4
t nanoSecond = 5
date := Date readFrom: '1-01-2005' readStream
time := Time readFrom: '0:00 am' readStream
(DateAndTime date: date time: time) printString = '2005-01-01T00:00:00+00:00'
(TimeStamp date: date time: time) printString = '1 January 2005 12:00 am'

'Delay'

d := Delay forSeconds: 5.
{
d delayDuration = 5000.
d beingWaitedOn = false.
d resumptionTime = nil.
}

(Delay forSeconds: 0.5) wait. Transcript nextPutAll: 'delayed'; cr.
(Delay forSeconds: 2/3) wait. Transcript nextPutAll: 'delayed'; cr.
(Delay forSeconds: 1) wait. Transcript nextPutAll: 'delayed'; cr.
[ d wait ] fork. [ [ d beingWaitedOn ] whileTrue: [Transcript show: d; cr. (Delay forSeconds: 0.5) wait ] ] fork

'Dictionary'

Dictionary isVariable not.                                 "St-80=true, Squeak=false"
Dictionary new add: #a -> 1; add: #b -> 2; yourself        "add: inserts associations"
Dictionary newFromPairs: #(#a 1 #b 2)                       "dictionary from adjacent key and value elements of array"
Dictionary newFrom: {#a -> 1. #b -> 2}                      "dictionary from associations in collection"
(Dictionary withAll: { #a -> 1. #b -> 2 }) any: 2           "select any two of"
d := Dictionary newFrom: {#a -> 1. #b -> 2. #c -> 3}
d isDictionary.
(d any: 2)
(d any: 2 as: Array)
d associationsDo: [ :each | Transcript show: each; cr ]
d do: [ :each | Transcript show: each; cr ]
d keysDo: [ :each | Transcript show: each; cr ]
d valuesDo: [ :each | Transcript show: each; cr ]
d keysAndValuesDo: [ :key :value | Transcript show: {key. value}; cr ]
d select: [ :each | each = 2 ]
d associationsSelect: [ :each | each = (#b -> 2) ]
d collect: [ :each | each * 2 ]
d includes: 2
d includesAssociation: (#b -> 2)
d includesIdentity: 2
d includesKey: #b
d includesAllOf: #(2 3)
d includesAllOf: (d any: 2)
d keys asBag = #(a b c) asBag
((Dictionary new: 2) fillFrom: #(3 4) with: [ :item | item -> item printString ])

'Dictionary ; Smalltalk'

{
(Smalltalk at: #Array) = Array.                             "lookup value in Smalltalk dictionary"
(Smalltalk at: #DoesNotExist ifAbsent: [ nil ]) = nil.       "lookup value with clause for absent values "
(Smalltalk includesKey: #DoesNotExist) = false.              "see if key exists in dictionary"
} allSatisfy: #yourself

Smalltalk keys size                                         "get set of all known keys "
Smalltalk halt

'Duration'

{
Duration zero seconds = 0.
Duration oneDay days = 1.
(Duration days: Time daysFromSmalltalkEpochToPosixEpoch) days = 25202.
}

'Error'

(Error new messageText: 'oh oh') printString
UnhandledError new defaultAction
Error signal
Error signal: 'oh oh'
[ 1 error: 'oh oh' ] on: Error do: [ :error | Transcript show: 'Error handler: answers: '; show: error; cr. true]
1 error: 'an integer?'
3.141 error: 'a float?'
#sym error: 'a symbol?'
Transcript error: 'a transcript?'
Smalltalk error: 'a smalltalk?'
([nil haltIf: true] on: Halt do: [ :err | 'Halted!' ]) = 'Halted!'

'Float'

{
3.141 isFloat = true.                                        "is floating point"
3.141 identityHash = 3.141 hash.                             "hash"
3.0 hash = 3 hash.                                           "hash of integral is integral hash"
Float epsilon <= (1 * (10 raisedTo: -12)).
1.0 < 1.0 successor.
1.0 > 1.0 predecessor.
(1.25 timesTwoPower: 5) = 40.
(Float readFrom: '3.141') = 3.141.
(Float readFrom: '-1.4' readStream) = -1.4.
Float nan ~~ Float nan.
Float nan ~= Float nan.
3.141 asIEEE32BitWord = 1078527525.
(Double fromIEEE32Bit: 1078527525) closeTo: 3.141.
} allSatisfy: #yourself

'Fraction'

{
(Fraction numerator: 2 denominator: 3) class = Fraction.
(2/3) isMemberOf: Fraction.
(Fraction readFrom: '5/4') = (5/4).
(Fraction numerator: 2 denominator: 3) reduced = (2/3).
2 / (3/4) = (8/3).
(2/3) + (2/3) = (4/3).
((2/3) + (1/2)) = (7/6).		"case showing reduction to common denominator"
((2/3) + (4/3)) = 2.		"case where result is reduced to an Integer"
(3 / 2) ceiling = 2.
(-3 / 2) ceiling = -1.
(4/3) denominator = 3.
((4/3) gcd: (7/5)) = (1/15).
((4/3) lcm: (7/5)) = 28.
(4/3) negated = (-4/3).
(4/3) negative not. "is negative?"
(4/3) numerator = 4.
((2/3) raisedToInteger: 5) = (32/243).		 "fractions also can be exponentiated"
(9 / 5) reciprocal = (5/9).
(7 / 5) squared = (49/25).
(3 / 2) truncated = 1.
(1/3) isAnExactFloat not.
(1/4) isAnExactFloat.
(1/4) asExactFloat = 0.25.
} allSatisfy: #yourself

4/9 raisedTo: 1/2 "2/3"
9/4 raisedTo: 1/2 "3/2"
8/27 raisedTo: 1/3 "2/3"
27/8 raisedTo: 1/3 "3/2"
32/243 raisedTo: 3/5 "8/27"
8/27 raisedTo: 5/3 "32/243"
(4/3) hash
Transcript show: (5 / 4); cr
(5 / 4) raisedToFraction: (7 / 5)
100 / 100.0 "1!"
(1/2) < 0.5 "false"
(1/3) > 0.25 "true"
0.5 < (1/2) "false"
0.25 < (1/3) "true"
0.5 < (1/4) "false"
0.5 < (1/2) "false"
0.5 < (3/4) "true"
0.5 <= (1/4) "false"
0.5 <= (1/2) "true"
0.5 <= (3/4) "true"
0.5 > (1/4) "true"
0.5 > (1/2) "false"
0.5 > (3/4) "false"
0.5 >= (1/4) "true"
0.5 >= (1/2) "true"
0.5 >= (3/4) "false"
0.5 = (1/4) "false"
0.5 = (1/2) "true"
0.5 = (3/4) "false"
0.5 ~= (1/4) "true"
0.5 ~= (1/2) "false"
0.5 ~= (3/4) "true"

'Generator'

g := Generator on: [ :g | 1 to: 9 do: [ :item | g yield: item ] ]. [ g atEnd ] whileFalse: [ Transcript show: g next; cr ]
g := Generator on: [ :g | g yield: 1. g error: 'error']. g next "propagate errors"
x := 0. o := ''. g := Generator on: [ :g | x := 1. g yield: 'A',x. x := 2. g yield: 'B',x. x := 3. g yield: 'C',x]. g do: [ :s | o := o, $(, x, $), s]. o "ghc = (3)A1(3)B2(3)C3"

'IdentityBag'

s := 'string'
c := s copy "s = c and: (s == c) not"
b := Bag new add: s; add: s; add: c; yourself
(b occurrencesOf: s) = 3 and: (b occurrencesOf: c) = 3
i := IdentityBag new add: s;  add: s; add: c; yourself
(i occurrencesOf: s) = 2 and: (i occurrencesOf: c) = 1

'Integer'

{
1 isInteger = true.
1 hash = 1 identityHash.
11 isPrime.
((1 to: 99) select: [ :each | each isPrime ]) size = 25.
(Integer primesUpTo: 4096) size = 564.
(Integer readFrom: '100') = 100.
(Integer readFrom: '-99' readStream) = -99.
40 lowBit = 4.
(0 digitAt: 1) = 0.
123 printString = '123'.
-987654321 printString = '-987654321'.
(987654321 significantDigits: 1) = 1000000000.
(987654321 significantDigits: 2) = 990000000.
123456789 asStringWithCommas = '123,456,789'.
-123456789 asStringWithCommas = '-123,456,789'.
4 / 2 = 2.                      "integer division with integer result"
} allSatisfy: #yourself

'Interval'

Interval from: 5 to: 10                                     "create interval object"
5 to: 10                                                    "interval from 5 to 10"
Interval from: 5 to: 10 by: 2                               "create interval object with specified increment"
5 to: 10 by: 2                                              "interval from 5 to 10 by 2"
(1 to: 5) isEmpty not                                       "test if empty"
(1 to: 5) size = 5                                          "number of elements"
(1 to: 9) includes: 9                                       "test if element is in collection"
(1 to: 9) do: [ :item | Transcript show: item; cr ]         "iterate over interval"
(1 to: 9) select: [ :item | item > 7 ]                      "return elements that pass test"
(1 to: 9) reject: [ :item | item < 2 ]                      "return elements that fail test"
(1 to: 9) collect: [ :item | item + item ]                  "transform each element"
(1 to: 9) detect: [ :item | item > 3 ] ifNone: []           "find position of first element that passes test"
(1 to: 9) inject: 0 into: [ :state :item | state + item ]   "sum elements"
(1 to: 9) asArray                                           "convert to array"
(1 to: 9) hash
(Interval newFrom: #(1 2 3)) = (1 to: 3)
(#(33 5 -23) as: Interval) = (33 to: -23 by: -28)
(-4 to: -12 by: -1) as: Interval
(#(2 4 6) as: Interval) = (2 to: 6 by: 2)
(1 to: 10) + 5 = (6 to: 15)
(6 to: 15) - 5 = (1 to: 10)
(1 to: 9) hash
(1 to: 9) ~= ((1 to: 9) asArray) "intervals are not equal to arrays"
(1 to: 9) hasEqualElements: ((1 to: 9) asArray) "intervals and arrays may have equal elements"
(1.0 to: 3.0 by: 1.0) includes: 3.0             "true"
(1.0 to: 3.0 by: 1.0 successor) includes: 3.0   "false"
(0 to: 1 by: 1/10) = (0.0 to: 1 by: 1/10)
(1 to: 4) permutationsDo: [ :each | Transcript show: each printString; cr ]
(10 to: 20 by: 3) reverseDo: [ :each | Transcript show: each printString; cr ]
(1 to: 2 by: 1/2) = ({1. 3/2. 2} as: Interval)
(1 to: 2 by: 1/5) = ({1. 6/5. 7/5. 8/5. 9/5. 2} as: Interval)
(2 to: 1) anyOne

'Message sends'

{
2 sqrt = 1.4142135623730951.                                "unary message"
(2 raisedTo: 10) = 1024.                                    "keyword message*"
(194 * 9) = 1746.                                           "binary message"
(Transcript show: (194 * 9) printString; cr) = Transcript.  "combination (chaining)"
(2 perform: #sqrt) = 2 sqrt.                                "indirect message send"
[ 1 unimplementedMessage ] on: Error do: [ true ].
6561 sqrt sqrt sqrt = 3.        "unary message sequence"
} allSatisfy: #yourself

'Method'

{
(1 class compiledMethodAt: #*) methodClass = SmallInteger.
((1 class compiledMethodAt: #*) invokeOn: 2 with: #(3)) = 6.
(1 class compiledMethodAt: #*) primitive = 9.
(SmallInteger >> #*) primitive = 9.
(3.14 class compiledMethodAt: #*) primitive = 49.
(1 class compiledMethodAt: #*) selector = #*.
} allSatisfy: #yourself

(1 class compiledMethodAt: #*) printString

'Month'

(Month readFrom: 'July 1998' readStream) printString = 'July 1998'.

'Number'

{
(Number readFrom: '42') = 42.
(Number readFrom: '3.14') = 3.14.
(23 printShowingDecimalPlaces: 2) = '23.00'.
(23.5698 printShowingDecimalPlaces: 2) = '23.57'.
(-234.567 printShowingDecimalPlaces: 5) = '-234.56700'.
(23.4567 printShowingDecimalPlaces: 0) = '23'.
(23.5567 printShowingDecimalPlaces: 0) = '24'.
(-23.4567 printShowingDecimalPlaces: 0) = '-23'.
(-23.5567 printShowingDecimalPlaces: 0) = '-24'.
(100000000 printShowingDecimalPlaces: 1) = '100000000.0'.
(0.98 printShowingDecimalPlaces: 5) = '0.98000'.
(-0.98 printShowingDecimalPlaces: 2) = '-0.98'.
(2.567 printShowingDecimalPlaces: 2) = '2.57'.
(-2.567 printShowingDecimalPlaces: 2) = '-2.57'.
(0 printShowingDecimalPlaces: 2) = '0.00'.
} allSatisfy: #yourself

'Object'

{
Object superclass = nil.
Object class class = Metaclass.
([ :a | { a } ] perform: #value: with: 1) = #(1).
([ :a :b | { a. b } ] perform: #value:value: with: 1 with: 2) = #(1 2).
([ :a :b :c | { a. b. c } ] perform: #value:value:value: with: 1 with: 2 with: 3) = #(1 2 3).
([ :a :b :c :d | { a. b. c. d } ] perform: #value:value:value:value: with: 1 with: 2 with: 3 with: 4) = #(1 2 3 4).
([ :a :b :c :d :e | { a. b. c. d. e } ] perform: #value:value:value:value:value: with: 1 with: 2 with: 3 with: 4 with: 5) = #(1 2 3 4 5).
([ :a :b :c :d :e | { a. b. c. d. e } ] perform: #value:value:value:value:value: withArguments: #(1 2 3 4 5)) = #(1 2 3 4 5).
} allSatisfy: #yourself

'OrderedCollection'

OrderedCollection isVariable                                "St-80 = true, Squeak = false"
OrderedCollection withAll: #(1 2 3 4)                       "Instantiate from collection"
((OrderedCollection with: #x) detect: [:x | x isSymbol]) = #x
OrderedCollection newFrom: #(1 2 3)
#(1 2 3) as: OrderedCollection
c := #('Jim' 'Mary' 'John' 'Alice' ) asOrderedCollection
c add: 'Mary' before: 'Mary'; asArray
OrderedCollection new add:true ; add: true ; add: true ;add: false ; add: false; yourself

'OrderedDictionary'

(OrderedDictionary new: 7) instVarNamed: #tally
(OrderedDictionary new: 7) instVarNamed: #order

'Primitive'

(SmallInteger methodDict at: #*) primitive = 9 "squeak"

'ReadStream'

(ReadStream on: 'Hello read stream') contents               "contents of ReadStream on String"
(ReadStream on: 'Hello read stream' from: 1 to: 5) contents "contents of ReadStream on String limited to 5 places"
s := (ReadStream on: 'Hello read stream')                   "ReadStream on string"
s position: 3                                               "set position"
s position = 3                                              "read position"
s next = $l                                                 "get next"
s peek = $o                                                 "peek next"
s contents                                                  "get all"
s atEnd = false                                             "test is at end of stream"
(s upTo: $s) = 'o read '                                    "read up to"
s reset                                                     "set position to zero"
s nextLine = 'Hello read stream' "read line"
'3.14' readStream peek = $3
ascii := String withAll: ((0 to: 255) collect: [:n| n asCharacter])
(ReadStream on: ascii from: $a asInteger + 1 to: $z asInteger + 1) contents = ($a to: $z)
$ isSeparator = true
' string' readStream skipSeparators upToEnd = 'string'

'ReadWriteStream'

s := ReadWriteStream on: 'ReadWriteStream' from: 1 to: 6
s contents = 'ReadWr'
s position = 0
s nextPut: $t
s nextPutAll: 'erse'
s nextPut: Character space
s contents = 'terse '
s atEnd = true
s reset
(ReadWriteStream on: 'Hello read write stream') contents = '' "gnu = 'Hello read write stream'"
(ReadWriteStream with: #(1 2)) contents = #(1 2)
ascii := String withAll: ((0 to: 255) collect: [:n| n asCharacter])
(ReadWriteStream on: ascii from: $a asInteger + 1 to: $z asInteger + 1) contents = ($a to: $z)

'SequenceableCollection'

#(1 2 3 4) permutationsDo: [ :each | Transcript show: each printString; cr ]
#(1 2 3 4) permutations asSet size = 24
(#(1 9 2 8 3 7 4 6) pairsCollect: [ :i :j | i + j ]) = #(10 10 10 10).
#(1 9 2 8 3 7 4 6) pairsDo: [ :i :j | Transcript show: i + j; cr ]
#(1 9 2 8 3 7 4 6) groupsDo: [ :i :j | Transcript show: i + j; cr ]

'Set'

{
(Set newFrom: {Character cr. Character lf}) size = 2.
(Set newFrom: #(1 2 3)) size = 3.
(Set withAll: #(1 2 2 3 3 3)) size = 3.
(#(1 2 3) as: Set) asArray = #(1 2 3).
(#(1 2 3) asSet copyWithout: 3) = #(1 2) asSet.
(#(1 2 2 3 3) asSet any: 2) = #(1 2) asSet.
((Set new: 2) fillFrom: #(9 16) with: #sqrt) = #(3 4) asSet.
(Set with: 1) printString = 'a Set(1)'.
} allSatisfy: #yourself

'SetElement (Set with nil)'

{
nil asSetElement class = SetElement.
1 asSetElement class = SmallInteger.
nil asSetElement ~~ nil.
nil asSetElement == nil asSetElement.
1 asSetElement == 1.
(Set new add: nil; add: nil; yourself) size = 1.
} allSatisfy: #yourself

'SmallInteger'

{
1 class = SmallInteger.
1 hash = 1 identityHash.
(12345 digitAt: 1) = 57.
40 lowBit = 4.
SmallInteger maxVal = 9223372036854775807.
SmallInteger maxVal = ((2 raisedTo: 63) - 1).
1 isMemberOf: SmallInteger.
1 decimalDigitLength = 1.
10 decimalDigitLength = 2.
(10 raisedTo: 8) decimalDigitLength = 9.
(10 raisedTo: 18) decimalDigitLength = 19.
SmallInteger maxVal decimalDigitLength = 19.
} allSatisfy: #yourself


'FortranNumberParser'

{
(FortranNumberParser parse: '42') = 42.
(FortranNumberParser parse: '3.141') = 3.141.
(FortranNumberParser parse: '-23') = -23.
(FortranNumberParser parse: '-0.01') = -0.01.
(FortranNumberParser parse: '1e3') = 1000.
(FortranNumberParser parse: '1e-3') = 0.001.
(FortranNumberParser parse: '-5e-2') = -0.05.
} allSatisfy: #yourself

'Scanner'

{
Scanner doItCharacter asInteger = 1114111.
Scanner typeTable size = 256.
Scanner isLiteralSymbol: #symbol.
} allSatisfy: #yourself

'SmalltalkNumberParser'

{
(SmalltalkNumberParser parse: '42') = 42.
(SmalltalkNumberParser parse: '3.141') = 3.141.
(SmalltalkNumberParser parse: '16rFF') = 255.
(SmalltalkNumberParser parse: '2r1000001') = 65.
(SmalltalkNumberParser on: '2/3') nextFraction = (2/3).
(SmalltalkNumberParser parse: 'Infinity') = Float infinity.
(SmalltalkNumberParser parse: 'NaN') isNaN.
(SmalltalkNumberParser parse: '-0.0') = Float negativeZero.
(SmalltalkNumberParser parse: '-Infinity') = Float negativeInfinity.
} allSatisfy: #yourself

'SortedCollection'

{
(#(4 2 7) as: SortedCollection) asArray = #(2 4 7).
((#($p $e $n) as: SortedCollection) as: String) = 'enp'.
('pen' as: SortedCollection) asArray = #($e $n $p).
(#pen as: SortedCollection) asArray = #($e $n $p).
(#(10 9 8 7 5 4 4 2) asSortedCollection asArray) = #(2 4 4 5 7 8 9 10).
((1 to: 5) asSortedCollection collect: [ :each | each ]) class = OrderedCollection. "collect generates an ordered collection"
} allSatisfy: #yourself

c := SortedCollection new sortBlock: [:a :b | a < b]; add: 'truite'; add: 'brochet'; yourself
c copyEmpty
c first = 'brochet'
cpy := c copy
cpy hasEqualElements: c
cpy species = c species
cpy sortBlock = c sortBlock
cpy first = 'brochet'
(1 to: 5) asSortedCollection add: 0; yourself
(1 to: 5) asSortedCollection collect: [ :each | each < 4]

'SortFunction'

{
(#((1 2) (2 3) (0 0)) sorted: #first ascending) = #((0 0) (1 2) (2 3)).
(#name ascending value: Array value: Set) = true.
(#sum ascending value: #(1 2) value: #(3 4)) = true.
(#min ascending value: #(1 2) value: #(3 4)) = true.
} allSatisfy: #yourself

'String'

{
String isVariable.                                           "St-80 = true, Squeak = false"
'string' isLiteral.                                                    "string literal"
'  text  ' withBlanksTrimmed = 'text'.                       "remove leading and trailing space"
'123' asInteger = 123.                                       "parse integer"
'12345' asNumber = 12345.                                    "parse integer or float"
'3.141' asNumber = 3.141.                                    "parse integer or float"
(String newFrom: #($P $e $n)) = 'Pen'.
(#($P $e $n) as: String) = 'Pen'.
('string' compare: 'symbol') = 1. "less than"
('string' compare: 'string') = 2. "equal"
('symbol' compare: 'string') = 3. "greater than"
'string' < 'symbol'. "less than"
'string' = 'string'. "equal"
'symbol' > 'string'. "greater than"
'string' copy = 'string' copy. "copied strings are equal"
'string' asString = 'string'.
'string' copy asString = 'string' copy.
'string' == 'string'. "string literals are identical"
'string' copy ~~ 'string' copy. "copied strings are not identical"
#('string') = {'string'}.
(String withAll: ($a to: $z)) = 'abcdefghijklmnopqrstuvwxyz'.
($a to: $z) = 'abcdefghijklmnopqrstuvwxyz'.
'string' hash = 'string' copy hash. "strings and copies of strings hash equally"
('' asIntegerSigned: True) = nil.
'' asInteger = nil.
'1796p-qR' asInteger = 1796.
'donald' asInteger = nil.
'abc234def567' asInteger = 234.
'-94' asInteger = -94.
'foo-bar-92' asInteger = -92.
('fred the bear' copyWithout: $e) = 'frd th bar'.
String empty == String empty.
String empty size = 0.
String new species = String new species.
'{\' = '{\'.
('foo {1} bar' indexOfAnyOf: String formatCharacterSet startingAt: 1) = 5.
'up:to:' keywords = #('up:' 'to:').
#up:to: keywords = #('up:' 'to:').
'copy:from:to:' keywords = #('copy:' 'from:' 'to:').
'up' keywords = #('up').
'at:' keywords = #('at:').
'string' includes: $i.           "true"
('string' includes: nil) = false.
String initializedInstance = ''.
} allSatisfy: #yourself

s := 'string' copy. s at: 4 put: $u. s = 'strung'. "=> strung, string literals are mutable"
'string' hash ~= #'string' hash "strings and symbols hash unequally"
'foo {1} bar' format: #(3.141)

'Symbol'

{
(#symbol isKindOf: String).
(#symbol isMemberOf: String) = false.
(#a , #b) = 'ab'.                                            "Symbol -> Symbol -> String"
(#symbol at: 3) = $m.                                        "Symbols are indexable but immutable"
[ #symbol at: 1 put: $S ] on: Error do: [ true ].                 "=> error"
(#perform:with: numArgs) = 2.                                "number of arguments of selector"
#perform:with: keywords = #('perform:' 'with:').             "keywords of selector as array of strings"
#symbol asString = 'symbol'.                                 "convert to string"
#symbol asSymbol = #'symbol'.                                "identity"
'symbol' asSymbol = #symbol.
'symbol' copy asSymbol = #symbol.
#+ isInfix.                                                  "is infix (binary) selector"
#perform: isKeyword.                                         "is keyword selector"
#symbol isSymbol.                                            "predicate"
'symbol' isSymbol = false.                                   "predicate"
(Symbol newFrom: #($P $e $n)) = #Pen.
(#($P $e $n) as: Symbol) = #Pen.
#testAnd isTestSelector = true.
#symbol species = String.
#(x y z) = {#x. #y. #z}.
#x copy = #x.
'x' asSymbol = #x.
'x' copy asSymbol = #x.
'a string' copy asSymbol = #'a string'.
#symbol hash = 'symbol' hash.    "false in gnu, true in squeak"
(#abs value: -1) = 1.                "implement value procotol for selectors"
#< value: 1 value: 2.            "implement value procotol for selectors"
#symbol capitalized = #Symbol.
Symbol initializedInstance = #''.
#four printString = '#four'.
} allSatisfy: #yourself

'Time'

{
Time epochOffset = 2177452800.
Time epochOffsetMicros = 2177452800000000.
(Time readFrom: (ReadStream on: '2:23:09 pm')) printString = '2:23:09 pm'.
(Time readFromString: '12:34:57') printString = '12:34:57 pm'.
(Time readFromString: '4:02:47.42 am') printString = '4:02:47.42 am'.
(Time readFrom: '0:00 am' readStream) printString = '12:00 am'.
} allSatisfy: #yourself

Time posixMicrosecondClockWithOffset "{1653874256296392. 36000}"
Time utcMicrosecondClock "3831326473237958"
Time dateAndTimeNow first printString "30 May 2022"
Time dateAndTimeNow second printString "10:47:11 am"
Time totalSeconds "3831362321"
Time now printString "10:47:30.644892 am"
Time microsecondsToRun: [ 28 benchFib ]
Time millisecondsToRun: [ 28 benchFib ]
Time durationToRun: [ 28 benchFib ]

'Timespan'

{
Timespan new printString = 'a Timespan(1901-01-01T00:00:00+00:00D0:00:00:00)'.
(Timespan starting: (DateAndTime nowAtOffset: Duration zero)) makeUTC <= Timespan current.
Timespan current <= (Timespan starting: (DateAndTime current)) makeUTC.
} allSatisfy: #yourself

date := Date readFrom: '1986-05-23' readStream
(Timespan starting: date) printString = 'a Timespan(1986-05-23T00:00:00+00:00D0:00:00:00)'
(Timespan starting: date duration: 365 days) end printString = '1987-05-22T23:59:59.999999999+00:00'
Timespan current printString "a Timespan(2022-05-31T02:44:10.055496+00:00D0:00:00:00)"
(Timespan starting: (DateAndTime year: 2005 month: 1 day: 1) duration: 1 week) makeUTC printString

'Transcript'

Transcript clear                                            "clear to transcript window"
Transcript show: 'Hello World'; cr                          "output string in transcript window"
Transcript nextPutAll: 'Hello World'; cr                    "output string in transcript window"
Transcript nextPut: $A                                      "output character in transcript window"
Transcript space                                            "output space character in transcript window"
Transcript tab                                              "output tab character in transcript window"
Transcript cr                                               "carriage return / linefeed"
'Hello' printOn: Transcript                                 "append print string into the window"
'Hello' storeOn: Transcript                                 "append store string into the window"
Transcript endEntry                                         "flush the output buffer"

'UndefinedObject'

{
nil hash = nil copy hash.
nil copy = nil.
nil copy == nil.
(nil ifNil: ['nil'] ifNotNil: ['notNil']) = 'nil'.
(#() ifNil: ['nil'] ifNotNil: ['notNil']) = 'notNil'.
} allSatisfy: #yourself

UndefinedObject initializedInstance class == UndefinedObject

'Week'

{
(Week nameOfDay: 1) = #Sunday.
Week dayNames = ((1 to: 7) collect: [ :each | Week nameOfDay: each ]).
} allSatisfy: #yourself

'WriteStream'

s := WriteStream on: (String new: 10)                       "WriteStream on String"
s nextPutAll: 'terse'                                       "put each item from colection into stream"
s position = 5                                              "get position"
s position: 5                                               "set position"
s contents = 'terse'                                        "get contents"
s size = 5                                                  "stream size"
s atEnd = true                                              "test if at end (read limit)"
s nextPut: $r                                               "put item on stream"
s contents = 'terser'
s := WriteStream on: (Array new: 10)                       "WriteStream on Array"
s nextPutAll: (1 to: 10)                                   "Put ten numbers into stream, the initial array is fill"
s nextPut: 11                                              "Put an eleventh number"
s size = 11                                                "The array is grown"
s := WriteStream on: #(1 2) copy
s nextPut: 3
s contents = #(3)
s := WriteStream on: String new
s nextPutAll: #($t $e $s $t)
s contents = 'test'

'Year'

{
Year current printString = 'a Year (2022)'.
1908 asYear daysInYear = 366.
1975 asYear daysInYear = 365.
} allSatisfy: #yourself

'Debugging'

nil yourself = nil                                          "returns receiver"
nil inspect                                                 "open object inspector window"
nil halt                                                    "breakpoint to halt interpreter"
nil halt: 'Reason'                                          "halt with indicateed text"
nil error: 'Reason'                                         "error with indicated text"
nil shouldNotImplement                                      "flag message should not be implemented"
nil subclassResponsibility                                  "flag message as abstract"
nil primitiveFailed                                         "system primitive failed"

Point allInstVarNames = #(x y).                             "instance variable names of class"
(Point instVarIndexFor: #x ifAbsent: []) = 1.               "index for named instance variable"
((Point x: 4 y: 2) instVarAt: 1) = 4.                       "lookup instance variable by index"
((Point x: 4 y: 2) instVarNamed: #x) = 4.                   "lookup instance variable by name"
CharacterSet allClassVarNames                               "class variable names of class"

'Methods'

(String respondsTo: #size) = true.                           "does an object implement a selector"
(Set allSelectors) size                                     "get all selectors an object responds to"
Array lookupSelector: #at:                                  "get method implementing selector, may be in superclass"
(Float compiledMethodAt: #sqrt)                             "get method implementing selector, must be in class"
(Array lookupSelector: #at:put:) class = CompiledMethod     "the object retrieved is a compiled method"
Number sourceCodeAt: #+                                     "get the source code for the selector"

'Class hierachy'

{
(nil isKindOf: Class) not.
Integer superclass = Number.                                 "discover the superclass of a class"
Integer subclasses includes: SmallInteger.
(ArrayedCollection subclasses includes: Symbol) not.
ArrayedCollection allSubclasses includes: Symbol.
Array allSuperclasses size = 4.                              "get sequence of all superclasses of a class"
(Set withAllSuperclasses) size = 4.                          "an ordered collection of a class and it's parents"
} allSatisfy: #yourself

Object allInstances                                         "get an array of all instances of a class"

'Benchmark'

28 benchFib = 1028457 "(result // seconds to run) = approx calls per second"
1 benchmark = 2 benchmark

(Time durationToRun: [ 28 benchFib ]) asMilliSeconds
(Time durationToRun: [ 2 benchmark ]) asMilliSeconds

Time microsecondsToRun: [ 28 benchFib ]
Time millisecondsToRun: [ 28 benchFib ]

Smalltalk tinyBenchmarks
Smalltalk keys
