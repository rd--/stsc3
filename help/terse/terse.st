'A Terse Guide to Smalltalk - c.f. http://wiki.squeak.org/squeak/5699 by Chris Rathman'

'Arithmetic expressions'

6 + 3 = 9                                                   "addition"
6 - 3 = 3                                                   "subtraction"
6 * 3 = 18                                                  "multiplication"
1 + 2 * 3 = 9                                               "evaluation always left to right (1 + 2) * 3"
(5 / 3) class                                               "division with non-intger result"
5.0 / 2.0 = 2.5                                             "division with float result (operand float)"
355.0 / 113                                                 "division with float result (operand integer)"
9 // 12 = 0                                                 "integer divide"
9 \\ 12 = 9                                                 "integer remainder (modulo)"
5.0 // 3.0 = 1                                              "integer divide"
5.0 \\ 3.0 = 2                                              "integer remainder"
-5 = 5 negated                                              "unary minus"
5 sign = 1                                                  "numeric sign (1, -1 or 0)"
5 negated = -5                                              "negate receiver"
1.25 integerPart = 1.0                                      "integer part of number"
1.25 fractionPart = 0.25                                    "fractional part of number"
5 reciprocal = (1/5)                                        "reciprocal function"
6 * 3.1 = 18.6                                              "auto convert to float"
5 squared = 25                                              "square function"
25 sqrt = 5.0                                               "square root"
(5 raisedTo: 2) = 25                                        "power function"
(5 raisedToInteger: 2) = 25                                 "power function with integer"
1 exp = Float e                                             "exponential"
-5 abs = 5                                                  "absolute value"
3.99 rounded = 4                                            "round"
3.99 truncated = 3                                          "truncate"
(3.99 roundTo: 1) = 4.0                                     "round to specified decimal places"
(3.99 truncateTo: 1) = 3.0                                  "truncate to specified decimal places"
3.99 floor = 3                                              "round down"
3.99 ceiling = 4                                            "round up"
5 factorial = 120                                           "factorial"
(-5 quo: 3) = -1                                            "integer divide rounded toward zero"
(-5 rem: 3) = -2                                            "integer remainder rounded toward zero"
(28 gcd: 12) = 4                                            "greatest common denominator"
(28 lcm: 12) = 84                                           "least common multiple"
Float e ln = 1.0                                            "natural logarithm"
1.0 exp ln = 1.0                                            "natural logarithm"
100 log = 2.0                                               "base 10 logarithm"
(10.0 raisedTo: 2.0) log = 2.0                              "base 10 logarithm"
(100 log: 10) = 2.0                                         "logarithm with specified base"
(100 floorLog: 10) = 2                                      "floor of the log"
Float radiansPerDegree = (Float pi / 180)                   "radians per degree"
180 degreesToRadians = Float pi                             "convert degrees to radians"
Float pi radiansToDegrees = 180.0                           "convert radians to degrees"
(Float pi / 2) sin = 1.0                                    "sine"
0.0 cos = 1.0                                               "cosine"
0.0 tan = 0.0                                               "tangent"
(10 max: 20) = 20                                           "get maximum of two numbers"
(10 min: 20) = 10                                           "get minimum of two numbers"
Float pi = 3.141592653589793                                "pi"
Float e = 2.718281828459045                                 "exp constant"
100 atRandom                                                "random integer in (1,self)"
1.0 atRandom                                                "random float in (0,self)"


'Array'

Array isVariable = true                                     "array is indexable"
(#(4 5 6 7) atAll: #(2 3)) = #(5 6)                         "at: for each index of array of indices"
3 timesRepeat: [ Transcript show: 'repeat' ]                "prints repeat three times, returns count"
#(1 2 3) do: [ :item | Transcript show: item ]              "prints 123"
(#(1 2 3 4 5) copyFrom: 2 to: 4) = #(2 3 4)                 "copy range"
x := {1. 2. 3}. {(x at: 2 put: $2) = $2. x = {1. $2. 3}}    "mutate slot, returns the value put"
#(1 2.3 '4' $5 #'6' #seven #(8 9) nil)                      "array literal syntax"
#(1 (2 (3 4))) = #(1 #(2 #(3 4)))                           "in st-80 sub-arrays do not require # prefix"
#(x x: nil) = #(#x #x: nil)                                 "in st-80 symbols do not require #prefix"
#(5 4 3 2 1) detect: [ :i | i < 3 ]                         "detect element (not index), so 2"

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

'Association'

Association key: #x value: 1                                "associate key with value"
#x -> 1 = (Association key: #x value: 1)                    "literal syntax"
(#x -> 1) key = #x                                          "key (left hand side) of association"
(#x -> 1) value = 1                                         "value (right hand side) of association"

'Behaviour'

Float selectors                                             "array of selectors implemented at class"
Float selectorsDo: [ :item | Transcript show: item; cr ]    "perform block at each selector"
Double includesSelector: #+                                 "test if class implements selector"
(Float includesSelector: #+) = false                        "does not look in superclasses"
Float canUnderstand: #+                                     "test if class can understand selector"
Float superclass = Number                                   "superclass of class"
Float inheritsFrom: Number                                  "immediate inheritance"
Float inheritsFrom: Magnitude                               "non-immediate inheritance"
Float withAllSuperclassesDo: [:c | Transcript show: c; cr]  "perform block at self and each superclass in turn"
Float allSelectors                                          "get all selectors an object responds to"


'Bitwise Manipulation'

(255 bitAnd: 15) = 15                                       "and bits"
(240 bitOr: 15) = 255                                       "or bits"
(255 bitXor: 240) = 15                                      "xor bits"
255 bitInvert = -256                                        "invert bits"
(15 bitShift: 4) = 240                                      "left shift"
(240 bitShift: -4) = 15                                     "right shift"
(128 bitAt: 7) = 0                                          "bit at position (0|1) [!Squeak]"
(255 allMask: 15) = true                                    "test if all bits set in mask set in receiver"
(255 anyMask: 15) = true                                    "test if any bits set in mask set in receiver"
(255 noMask: 15) = false                                    "test if all bits set in mask clear in receiver"

'Booleans'

1 = 1 = true                                                "equality predicate (operator)"
1 ~= 2 = true                                               "equality predicate (operator)"
(1 == 1)                                                    "identical"
1 ~~ 2                                                      "not identical"
2 > 1                                                       "greater than"
1 < 2                                                       "less than"
1 >= 1                                                      "greater than or equal"
1 <= 1                                                      "less than or equal"
false not                                                   "boolean not"
true & false = false                                        "logical and (operator)"
true | false = true                                         "logical or (operator)"
(true and: false) = false                                   "logical and"
(true or: false) = true                                     "logical or"
(false and: ['and' error]) = false                          "logical and (short-circuit)"
(true or: ['or' error]) = true                              "logical or"
true xor: false                                             "test if one true and other false"
b := 5 between: 3 and: 12.                                  "between (inclusive)"
123 isKindOf: Number                                        "test if object is class or subclass of"
123 isMemberOf: SmallInteger                                "test if object is type of class"
123 respondsTo: #sqrt                                       "test if object responds to message"
true = true                                                 "constant"
false = false                                               "constant"
true not not = true                                         "not"
nil isNil = true                                            "test if object is nil"
1 positive = true                                           "test if number is positive"
0 strictlyPositive = false                                  "test if number is greater than zero"
-1 negative = true                                          "test if number is negative"
2 even = true                                               "test if number is even"
1 odd = true                                                "test if number is odd"
1 isInteger = true                                          "test if object is integer"
1.0 isFloat = true                                          "test if object is float"
1.0 isNumber = true                                         "test if object is number"
$A isUppercase = true                                       "test if upper case character"
$a isLowercase = true                                       "test if lower case character"
false asBit = 0
true asBit = 1
true asInteger > false asInteger

'Character'

Character space = $ 
Character tab = $	
Character newLine = Character nl
Character newPage asInteger = 12
(Character value: 97) = $a
$a asInteger = 97
$A asInteger = 65
$Z asInteger = 90
$a = $a
$a ~= $b
$1 digitValue = 1
$c isCharacter = true
$a isLetter = true
$1 isDigit = true
$a isLowercase = true
Character space isSeparator = true
$e isVowel
$a asUppercase = $A
$A asLowercase = $a
$a hash = $a asInteger hash

'CharacterSet'

CharacterSet ascii size = 127                               "ascii character set"
CharacterSet crlf                                           "carriage return and line feed"

'Collection'

#(1 3 2 9 4 5 7 8 6) max = 9                                "maximum item"
#(1 3 2 9 4 5 7 8 6) includes: 6                            "includes element"
#(1 3 2 9 4 5 7 8 6) includesAnyOf: #(0 6)                  "includes any element of a collection"

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


'Conversion'

3.99 asInteger = 3                                          "convert number to integer (truncates in Squeak)"
3.99 asFraction                                             "convert number to fraction (399/100)"
3 asFloat                                                   "convert number to float"
3 asFloat = 3                                               "3 asFloat is equal to 3"
65 asCharacter = $A                                         "convert integer to character"
$A asciiValue = 65                                          "convert character to integer"
3.99 printString = '3.99'                                   "convert object to string via printOn:"
3.99 storeString = '3.99'                                   "convert object to string via storeOn:"
(15 radix: 16) = '16rF'                                     "convert to string in given base"

'Dictionary'

Dictionary new add: #a -> 1 ; add: #b -> 2 ; yourself       "add: inserts associations"
Dictionary newFromPairs: #(#a 1 #b 2)                       "dictionary from adjacent key and value elements of array"
Dictionary newFromAssociations: {#a -> 1. #b -> 2}          "dictionary from associations in array"
Dictionary isVariable                                       "St-80=true, Squeak=false"

'Dictionary ; Smalltalk'

(Smalltalk at: #SinOsc) = SinOsc                            "lookup value in Smalltalk dictionary"
(Smalltalk at: #DoesNotExist ifAbsent: [ nil ]) = nil       "lookup value with clause for absent values "
(Smalltalk includesKey: #DoesNotExist) = false              "see if key exists in dictionary"
Smalltalk keys size                                         "get set of all known keys "

'Error'

1 error: 'an integer?'
3.141 error: 'a float?'
#sym error: 'a symbol?'
Transcript error: 'a transcript?'
Smalltalk error: 'a smalltalk?'

'Float'

3.141 isFloat = true                                        "is floating point"
3.141 identityHash = 3.141 hash                             "hash"
3.0 hash = 3 hash                                           "hash of integral is integral hash"

'Integer'

1 isInteger = true
1 hash
1 identityHash

'Interval'

Interval from: 5 to: 10                                     "create interval object"
5 to: 10                                                    "interval from 5 to 10"
Interval from: 5 to: 10 by: 2                               "create interval object with specified increment"
5 to: 10 by: 2                                              "interval from 5 to 10 by 2"
(1 to: 5) isEmpty                                           "test if empty"
(1 to: 5) size                                              "number of elements"
(1 to: 9) includes: 9                                       "test if element is in collection"
(1 to: 9) do: [ :item | Smalltalk print: item ]             "iterate over interval"
(1 to: 9) select: [ :item | item > 7 ]                      "return elements that pass test"
(1 to: 9) reject: [ :item | item < 2 ]                      "return elements that fail test"
(1 to: 9) collect: [ :item | item + item ]                  "transform each element"
(1 to: 9) detect: [ :item | item > 3 ] ifNone: []           "find position of first element that passes test"
(1 to: 9) inject: 0 into: [ :state :item | state + item ]   "sum elements"
(1 to: 9) asArray                                           "convert to array"
(1 to: 9) hash

'Method calls'

2 sqrt                                                      "unary message"
2 raisedTo: 10                                              "keyword message*"
194 * 9                                                     "binary message"
Transcript show: (194 * 9) printString; cr                  "combination (chaining)"
2 perform: #sqrt                                            "indirect method invocation"
Transcript show: 'hello '; show: 'world'; cr                "cascading, send multiple messages to receiver"
3 + 2; * 100                                                "=> 300, sends message to same receiver not answer"

'OrderedCollection'

OrderedCollection isVariable                                "St-80 = true, Squeak = false"

'Set'

Set newFrom: {Character cr. Character lf}

'SmallInteger'

1 class = SmallInteger
1 hash = 1 identityHash


'Stream'

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
s nextLine                                                  "read line"

s := WriteStream on: (String new: 10)                       "WriteStream on String"
s nextPutAll: 'terse'                                       "put each item from colection into stream"
s position = 5                                              "get position"
s position: 5                                               "set position"
s contents = 'terse'                                        "get contents"
s size = 5                                                  "stream size"
s atEnd = true                                              "test if at end (read limit)"
s nextPut: $r                                               "put item on stream"
s contents = 'terser'

s := ReadWriteStream on: 'ReadWrite' copy from: 1 to: 6
s contents = 'ReadWr'
s position = 0
s nextPut: $t
s nextPutAll: 'erse'
s nextPut: Character space
s contents = 'terse '
s atEnd = true
s reset

(ReadWriteStream on: 'Hello read write stream' copy) contents


'Strings'

String isVariable                                           "St-80 = true, Squeak = false"
'string'                                                    "string literal"
'  text  ' withBlanksTrimmed                                "remove leading and trailing space"
s := 'string' copy. s at: 4 put: $u. s                      "=> strung, string literals are mutable"
'123' asInteger = 123                                       "parse integer"
'12345' asNumber = 12345                                    "parse integer or float"
'3.141' asNumber = 3.141                                    "parse integer or float"


'Symbols'

(#a , #b) = 'ab'                                            "Symbol -> Symbol -> String"
(#symbol at: 3) = $m                                        "Symbols are indexable but immutable"
#x at: 1 put: $y                                            "=> error"
(#perform:with: numArgs) = 2                                "number of arguments of selector"
#perform:with: keywords = #('perform:' 'with:')             "keywords of selector as array of strings"
#symbol asString = 'symbol'                                 "convert to string"
#symbol asSymbol = #'symbol'                                "identity"
#+ isInfix                                                  "is infix (binary) selector"
#perform: isKeyword                                         "is keyword selector"
#symbol isSymbol                                            "predicate"
'symbol' isSymbol = false                                   "predicate"

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

'Debugging'

nil yourself = nil                                          "returns receiver"
nil inspect                                                 "open object inspector window"
nil halt                                                    "breakpoint to halt interpreter"
nil halt: 'Reason'                                          "halt with indicateed text"
nil error: 'Reason'                                         "error with indicated text"
nil shouldNotImplement                                      "flag message should not be implemented"
nil subclassResponsibility                                  "flag message as abstract"
nil primitiveFailed                                         "system primitive failed"

'Methods'

(String respondsTo: #size) = true                           "does an object implement a selector"
(Set allSelectors) size                                     "get all selectors an object responds to"
Array lookupSelector: #at:                                  "get method implementing selector, may be in superclass"
(Float compiledMethodAt: #sqrt)                             "get method implementing selector, must be in class"
(Array lookupSelector: #at:put:) class = CompiledMethod     "the object retrieved is a compiled method"
Number sourceCodeAt: #+                                     "get the source code for the selector"

'Class hierachy'

Integer superclass                                          "discover the superclass of a class"
Object allInstances                                         "get an array of all instances of a class"
Array allSuperclasses                                       "get sequence of all superclasses of a class"
(Set withAllSuperclasses) size                              "an ordered collection of a class and it's parents"
