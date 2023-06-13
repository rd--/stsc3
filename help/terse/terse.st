'A Terse Guide to Smalltalk - c.f. http://wiki.squeak.org/squeak/5699 by Chris Rathman'

'Arithmetic expressions'
6 + 3 = 9 "addition"
6 - 3 = 3 "subtraction"
6 * 3 = 18 "multiplication"
1 + 2 * 3 = 9 "evaluation always left to right (1 + 2) * 3"
1 + 2 * 3 = ((1 + 2) * 3) "equals predicate is also left to right"
6 / 3 = 2 "integer division with integer result"
1 + (2 * 3) = 7 "parentheses group sub-expressions"
(5 / 3) isInteger not "division with fractional result"
5.0 / 2.0 = 2.5 "division with float result"
((5 / 3) asFloat) closeTo: 1.666666 "fraction as float"
(355 / 113) closeTo: 3.141592 "division with float result (operand integer)"
9 // 12 = 0 "integer division operator"
(21232729 // 1379) asFloat = 15397 "integer division operator"
(21232729 / 1379) asFloat > 15397 "division operator"
9 \\ 12 = 9 "integer remainder (modulo)"
50 // 30 = 1 "integer divide"
50 \\ 30 = 20 "integer remainder"
-5 = 5 negated "unary minus"
5 sign = 1 "numeric sign (1, -1 or 0)"
5 negated = -5 "negate receiver (unary minus)"
1.25 integerPart = 1.0 "integer part of number"
1.25 fractionPart = 0.25 "fractional part of number"
5 reciprocal = (1/5) "reciprocal function"
6 * 31 = 186 "auto convert to float"
5 squared = 25 "square function"
25 sqrt = 5.0 "square root"
(5 raisedTo: 2) = 25 "power function"
(5 raisedToInteger: 2) = 25 "power function with integer"
1 exp = Float e "exponential"
-5 abs = 5 "absolute value"
3.99 rounded = 4 "round"
3.99 truncated = 3 "truncate"
(3.99 roundTo: 1) = 4.0 "round to specified decimal places"
(3.99 truncateTo: 1) = 3.0 "truncate to specified decimal places"
3.99 floor = 3 "round down"
3.99 ceiling = 4 "round up"
5 factorial = 120 "factorial"
(-5 quo: 3) = -1 "integer divide rounded toward zero"
(-5 rem: 3) = -2 "integer remainder rounded toward zero"
(28 gcd: 12) = 4 "greatest common denominator"
(28 lcm: 12) = 84 "least common multiple"
Float e ln = 1.0 "natural logarithm"
1.0 exp ln = 1.0 "natural logarithm"
100 log = 2.0 "base 10 logarithm"
(10.0 raisedTo: 2.0) log = 2.0 "base 10 logarithm"
(100 log: 10) = 2.0 "logarithm with specified base"
(100 floorLog: 10) = 2 "floor of the log"
Float radiansPerDegree = (Float pi / 180) "radians per degree"
180 degreesToRadians = Float pi "convert degrees to radians"
Float pi radiansToDegrees = 180.0 "convert radians to degrees"
(Float pi / 2) sin = 1.0 "sine"
0.0 cos = 1.0 "cosine"
0.0 tan = 0.0 "tangent"
(10 max: 20) = 20 "get maximum of two numbers"
(10 min: 20) = 10 "get minimum of two numbers"
Float pi = 3.141592653589793 "pi"
Float e = 2.718281828459045 "exp constant"
100 atRandom between: 1 and: 100 "random integer in (1,self)"
0 isZero
1 isZero not
[ 1 / 0 ] ifError: [ true ] "divide by zero"
(5 / 3) class = Fraction "integer division with non-integer result"
(2 / 4) isFraction "integer division, fractional answer"
4 + 5 * 6 = 54 "operators are evaluated left to right"

'ArrayedCollection'
#(3 2 1) copy sort = #(1 2 3) "merge sort"
(9 to: 1 by: -1) asArray sort = (1 to: 9) asArray "merge sort"
(1 to: 99) asArray shuffle sort = (1 to: 99) asArray "merge sort"
(1 to: 99) asArray shuffle quickSort = (1 to: 99) asArray
'string' copy quickSort = 'ginrst'
'quick' copy quickSort = 'cikqu'
'merge' copy sort = 'eegmr' "merge sort"
'mergeSortFrom:to:by:' copy sort = ':::FSbeegmmooorrrtty' "merge sort"
'quickSort' copy quickSort = 'Scikoqrtu'
(#(5 4 3 2 1) copy sort: [ :lhs :rhs | lhs < rhs ]) = #(1 2 3 4 5)
| s | s := 'abcdefghijklmnopqrstuvwxyz'. s = ($a to: $z)
| s | s := ($a to: $z). s quickSort = s
| s | s := ($a to: $z). s sort = s "merge sort"
| s | s := ($a to: $z). s reverse quickSort = s
| s | s := ($a to: $z). s reverse sort = s "merge sort"
| s | s := ($a to: $z). s reverse asArray sort = s asArray " merge sort"
| s | s := ($a to: $z). s shuffle sort = s

'Array'
Array isVariable = true "array is indexable"
(Array withAll: #(1 2 3 4)) = #(1 2 3 4) "array from collection"
(#(4 5 6 7) atAll: #(2 3)) = #(5 6) "at: for each index of array of indices"
(#(1 2 3 4 5) copyFrom: 2 to: 4) = #(2 3 4) "copy range"
(#(1 2 3 4 5) copyFrom: 1 to: 5) = #(1 2 3 4 5) "copy range"
#(1 (2 (3 4))) = #(1 #(2 #(3 4))) "in st-80 sub-arrays do not require # prefix"
(#(5 4 3 2 1) detect: [ :i | i < 3 ]) = 2 "detect element (not index), so 2"
(#(1 2 3 4 5) first: 3) = #(1 2 3)
(#(1 2 3 4 5) take: 2) = #(1 2)
(#(1 2) take: 5) = #(1 2)
#(1 2) == #(1 2) "literals are identical"
#(1 2) ~~ #(1 2) shallowCopy "copies are not identical"
{1. 2} ~~ {1. 2} "array expressions are not identical"
{1. 2} ~~ {1. 2} shallowCopy "also when copied"
({1. 2. 3} at: 1 put: '1') = '1' "array expressions are mutable"
#(1 2 3) == #(1 2 3) "array literals are identical"
#(1 2 3) ~~ {1. 2. 3} "array literals and array expressions are not identical"
[ Array new: -1 ] on: Error do: [ true ] "error"
(#(1 2 3 4 5) any: 3) = #(1 2 3) "any three are first three"
((Array new: 2) fillFrom: #(3 4) with: [ :n | n * n ]) = #(9 16)
((Array new: 2) fillFrom: #(9 16) with: #sqrt) = #(3 4)
#() == #() "Empty array is canonical (identical to itself)"
| c | c := {'s'. #s. 3}. { (c at: 1) = 's'. (c at: 2) = #s. (c at: 3) = 3.} allSatisfy: #yourself

'Assignment'
| x | x := 5. x = 5 "assignment"
| x y z | x := y := z := 6. x = z "compound assignment"
| x y | x := (y := 6) + 1. x = (y + 1) "assignment value, y = 6, x = 7"

'Association'
(Association key: #x value: 1) class = Association "associate key with value"
#x -> 1 = (Association key: #x value: 1) "arrow message at Object"
(#x -> 1) key = #x "key (left hand side) of association"
(#x -> 1) value = 1 "value (right hand side) of association"
| a | a:= (#x -> 1). a = a copy
(({#x -> 1. #y -> 2} as: Dictionary) at: #x) = 1
(#x -> 1) printString = '#x->1'
(#+ -> 1) printString = '#+->1'

'Behaviour'
Float selectors size >= 50 "array of selectors implemented at class"
Float allSelectors size >= 300 "get all selectors an object responds to"
(Float canUnderstand: #+) "test if class can understand selector"
Float superclass = Number "superclass of class"
Float inheritsFrom: Number "immediate inheritance"
Float inheritsFrom: Magnitude "non-immediate inheritance"

'Bitwise Manipulation'
(255 bitAnd: 15) = 15 "and bits"
(240 bitOr: 15) = 255 "or bits"
(255 bitXor: 240) = 15 "xor bits"
255 bitInvert = -256 "invert bits"
(15 bitShift: 4) = 240 "left shift"
(240 bitShift: -4) = 15 "right shift"
(128 bitAt: 7) = 0 "bit at position (0|1) [!Squeak]"
(255 allMask: 15) "test if all bits set in mask set in receiver"
(255 anyMask: 15) "test if any bits set in mask set in receiver"
(255 noMask: 15) not "test if all bits set in mask clear in receiver"

'BlockClosure'
[ 42 ] class = BlockClosure
([ :aNumber | aNumber * aNumber ] value: 3) = 9 "apply block to one argument"
([ :aNumber | aNumber * aNumber ] cull: 3) = 9 "apply block with or without argument"
([ 42 ] cull: 3) = 42 "apply block with or without argument"
[ [ 42 ] value: nil ] on: Error do: [ true ] "error"
[ [ 42 ] value: nil ] ifError: [ true ] "error"
[ [ :item | 42 ] value ] on: Error do: [ true ] "error"
[ [ :item | 42 ] value ] ifError: [ true ] "error"
[ 42 ] ~= 42
| b | b := [ :x | x * 2 ]. b = b copy
| b | b := [ :x | x * 2 ]. (b copy value: 2) = 4

'Booleans'
true = true "constant"
false = false "constant"
1 = 1 = true "equality predicate (operator)"
1 ~= 2 = true "inequality predicate (operator)"
(1 == 1) = true "identical"
(1 ~~ 2) = true "not identical"
2 > 1 "greater than"
1 < 2 "less than"
1 >= 1 "greater than or equal"
1 <= 1 "less than or equal"
false not "boolean not"
true not not = true "not"
true & false = false "logical and (operator)"
true | false = true "logical or (operator)"
(true and: false) = false "logical and"
(true or: false) = true "logical or"
(false and: ['and' error]) = false "logical and (short-circuit)"
(true or: ['or' error]) = true "logical or"
true xor: false "test if one true and other false"
5 between: 3 and: 12 "between (inclusive)"
123 isKindOf: Number "test if object is class or subclass of"
123 isMemberOf: SmallInteger "test if object is type of class"
123 respondsTo: #sqrt "test if object responds to message"
true = true "constant"
false = false "constant"
true not not = true "not"
nil isNil = true "test if object is nil"
1 positive = true "test if number is positive"
0 strictlyPositive = false "test if number is greater than zero"
-1 negative = true "test if number is negative"
2 even = true "test if number is even"
1 odd = true "test if number is odd"
1 isInteger = true "test if object is integer"
1.0 isFloat = true "test if object is float"
1.0 isNumber = true "test if object is number"
$A isUppercase = true "test if upper case character"
$a isLowercase = true "test if lower case character"
false asBit = 0
true asBit = 1
true asInteger > false asInteger

'ByteArray'
(ByteArray new: 4) hex = '00000000'
(ByteArray new: 4) size = 4
((ByteArray new: 4) at: 1) = 0
((ByteArray new: 4) byteAt: 2) = 0
((ByteArray new: 4) at: 1 put: 15; at: 3 put: 240; hex) = '0f00f000'
((ByteArray new: 4) byteAt: 2 put: 15; byteAt: 4 put: 240; hex) = '000f00f0'
(ByteArray newFrom: #(1 2 3 4)) hex = '01020304'
((ByteArray new: 1) readHexFrom: '01' readStream) hex = '01'
(ByteArray readHexFrom: '0f00f000') hex = '0f00f000'
'string' asByteArray hex = '737472696e67'
(ByteArray readHexFrom: '737472696e67') asString = 'string'
((ByteArray new: 4) atAllPut: 15) hex = '0f0f0f0f'
'string' asByteArray asArray = #(115 116 114 105 110 103)

'Character'
$ = Character space
$	 = Character tab
Character linefeed = Character lf
Character tab asInteger = 9
Character nl asInteger = 10
Character linefeed asInteger = 10
Character newPage asInteger = 12
Character cr asInteger = 13
$\ asInteger = 92
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
$s ~= nil
($a to: $z) = 'abcdefghijklmnopqrstuvwxyz'
('/' , Character separators) size = (Character separators size + 1)
$a printString = '$a'
$5 printString = '$5'
$@ printString = '$@'
${ = ${
$\ = $\
$/ = $/
${ asciiValue = 123
$} asciiValue = 125

'Collection'
#() isEmpty = true "is collection empty"
#(1 2 3) = #(1 2 3) "are collections equal"
#(1 2 3) size = 3 "size of collection"
#(9 4 5 7 8 6) max = 9 "maximum item"
#(9 4 5 7 8 6) min = 4 "minimum element"
#(9 4 5 7 8 6) sum = 39 "sum of collection"
#(9 4 5 7 8 6) product = 60480 "product of collection"
#(1 3 2 9 4 5 7 8 6) includesAnyOf: #(0 6) "includes any element of a collection"
(#(1 2 2 3 3 3) histogramOf: [ :each | each]) = #(1 2 2 3 3 3) asBag
(#(2 3 4 5 5 6) copyWithout: 5) = #(2 3 4 6)
[ #(1 2) take: -1 ] on: Error do: [ true ]
(#() select: [ :each | each > 0 ] ) = #()

'Conditional Statements'
(true ifTrue: [ 'T' ]) = 'T' "if then"
(true ifFalse: [ 'F' ]) = nil "if not then"
(true ifTrue: [ 'T' ] ifFalse: [ 'F' ]) = 'T' "if then else"

'Constants'
true isBoolean "true constant"
false isBoolean "false constant"
nil isNil "nil object constant"
1 isInteger "integer constants"
3.14 isFloat "float constants"
2e-2 isFloat "fractional constants"
16r0F = 15 "hex constant"
-1 isInteger "negative constants"
'Hello' isString "string constant"
'I''m here' isString "single quote escape"
$A isCharacter "character constant"
$  isCharacter "character constant (space)"
#aSymbol isSymbol "symbol constants"
#(3 2 1) isArray "array constants"
#('abc' 2 $a) isArray "mixing of types allowed"

'Conversion'
3 asFloat = 3.0 "convert number to float"
3 asFloat = 3.0 "3 asFloat is equal to 3"
65 asCharacter = $A "convert integer to character"
$A asciiValue = 65 "convert character to integer"
3.99 printString = '3.99' "convert object to string via printOn:"
3.99 storeString = '3.99' "convert object to string via storeOn:"
3.99 asFraction isFraction "convert number to fraction (399/100)"

'Dictionary ; Smalltalk'
(Smalltalk at: #Array) = Array "lookup value in Smalltalk dictionary"
(Smalltalk at: #Array ifPresent: [ :class | class ]) = Array
(Smalltalk at: #DoesNotExist ifAbsent: [ nil ]) = nil "lookup value with clause for absent values "
(Smalltalk includesKey: #DoesNotExist) = false "see if key exists in dictionary"

'Duration'
Duration zero seconds = 0
(Duration seconds: 2) seconds = 2
(Duration seconds: 2.5) seconds = 2.5

'Error'
[ 1 error: 'divide by zero' ] ifError: [ true ]
[ 1 / 0 ] ifError: [ true ]

'Floating point'
1.0 * 3.0 > 3.0 = false "integral floating point math"
0.1 * 3.0 > 0.3 = true "general floating point math"
1.0 * 3.0 = 3.0 = true "integral floating point math"
0.1 * 3.0 = 0.3 = false "general floating point math"
1.0 + 1.0 + 1.0 = 3.0 = true "integral floating point math"
0.1 + 0.1 + 0.1 = 0.3 = false "general floating point math"
3.141 isFloat = true "is floating point"
3.0 hash = 3 hash "hash of integral is integral hash"
1.0 < 1.0 successor
1.0 > 1.0 predecessor
(1.25 timesTwoPower: 5) = 40
(Float readFrom: '-1.4' readStream) = -1.4
3.141 asIEEE32BitWord = 1078527525

'Fraction'
(Fraction numerator: 2 denominator: 3) class = Fraction
(2/3) isMemberOf: Fraction
(Fraction numerator: 2 denominator: 3) reduced = (2/3)
2 / (3/4) = (8/3)
(2/3) + (2/3) = (4/3)
((2/3) + (1/2)) = (7/6) "case showing reduction to common denominator"
((2/3) + (4/3)) = 2 "case where result is reduced to an Integer"
(3 / 2) ceiling = 2
(-3 / 2) ceiling = -1
(4/3) denominator = 3
((4/3) gcd: (7/5)) = (1/15)
((4/3) lcm: (7/5)) = 28
(4/3) negated = (-4/3)
(4/3) negative not "is negative?"
(4/3) numerator = 4
((2/3) raisedToInteger: 5) = (32/243) "fractions also can be exponentiated"
(9 / 5) reciprocal = (5/9)
(7 / 5) squared = (49/25)
(3 / 2) truncated = 1
100 / 100.0 = 1
(1/2) < 0.5 = false
(1/3) > 0.25
0.5 < (1/2) = false
0.25 < (1/3)
0.5 < (1/4) = false
0.5 < (1/2) = false
0.5 < (3/4)
0.5 <= (1/4) = false
0.5 <= (1/2)
0.5 <= (3/4)
0.5 > (1/4)
0.5 > (1/2) = false
0.5 > (3/4) = false
0.5 >= (1/4)
0.5 >= (1/2)
0.5 >= (3/4) = false
0.5 = (1/4) = false
0.5 = (1/2)
0.5 = (3/4) = false
0.5 ~= (1/4)
0.5 ~= (1/2) = false
0.5 ~= (3/4)

'Integer'
1 isInteger = true
1 hash = 1 identityHash
(Integer readFrom: '-99' readStream) = -99
40 lowBit = 4
(0 digitAt: 1) = 0
123 printString = '123'
-987654321 printString = '-987654321'
4 / 2 = 2 "integer division with integer result"
255 printString = '255'
8r77 = 63
10r99 = 99
16rFF = 255
(1 to: 63 collect: [:i | ((-1 bitShift: i) bitShift: i negated) = -1 ]) allSatisfy: #yourself
(1 to: 62 collect: [:i | ((1 bitShift: i) bitShift: i negated) = 1 ]) allSatisfy: #yourself

'Interval'
(Interval from: 5 to: 10) class = Interval "create interval object"
(Interval from: 5 to: 10) = (5 to: 10) "interval from 5 to 10"
(Interval from: 5 to: 10 by: 2) last = 9 "create interval object with specified increment"
(5 to: 10 by: 2) last = 9 "interval from 5 to 10 by 2"
(1 to: 5) isEmpty not "test if empty"
(1 to: 5) size = 5 "number of elements"
(1 to: 9) includes: 9 "test if element is in collection"
((1 to: 9) select: [ :item | item > 7 ]) = #(8 9) "return elements that pass test"
((1 to: 9) reject: [ :item | item < 7 ]) = #(7 8 9) "return elements that fail test"
((1 to: 9) collect: [ :item | item + item ]) last = 18 "transform each element"
((1 to: 9) detect: [ :item | item > 3 ] ifNone: []) = 4 "find position of first element that passes test"
((1 to: 9) inject: 0 into: [ :sum :item | sum + item ]) = 45 "sum elements"
(1 to: 9) asArray = #(1 2 3 4 5 6 7 8 9) "convert to array"
(1 to: 9) ~= ((1 to: 9) asArray) "intervals are not equal to arrays"
(1.0 to: 3.0 by: 1.0) includes: 3.0
((1.0 to: 3.0 by: 1.0 successor) includes: 3.0) not
(0 to: 1 by: 1/10) = (0.0 to: 1 by: 1/10)
(9 to: 1 by: -1) asArray = #(9 8 7 6 5 4 3 2 1)
(5 to: 1) asArray = #()
(5 downTo: 1) asArray = #(5 4 3 2 1)
(5 downTo: 1 by: 2) asArray = #(5 3 1)

'Iteration'
| n | n := 0. 4 timesRepeat: [ n := n + 1 ]. n = 4 "times repeat loop (int)"
| n | n := 0. 4 do: [ :x | n := n + x ]. n = 10 "times repeat loop (int)"
| n | n := 0. 4 do: [ :x | n := n + x ]. n = 10 "for loop (int)"
| s | s := ''. 4 do: [ :x | s := s , x asString ]. s = '1234' "for loop (int)"
| s | s := ''. 1 to: 5 do: [ :x | s := s , x asString ]. s = '12345' "for loop (interval)"
| s | s := ''. (1 to: 3) reverseDo: [ :x | s := s , x asString ]. s = '321' "for loop (interval, reversed)"
| s | s := ''. #(1 3 5) do: [ :x | s := s , x asString ]. s = '135' "for loop (collection)"
| n | n := 9. [ n > 3 ] whileTrue: [ n := n - 1 ]. n = 3 "while loop"

'Magnitude'
1 < 3 = true "less than"
3 <= 3 = true "less than or equal to"
3 > 1 = true "greater than"
1 >= 1 = true "greater than or equal to"
2 = 2 = true "equal to"

'Math'
-3 abs = 3 "absolute value"
1.5 ceiling = 2 "ceiling (round up)"
0 cos = 1 "cosine"
180 degreesToRadians = Float pi "degreesToRadians"
2 even = true "eveness predicate"
1 exp closeTo: 2.718281828459045 "base e exponent function"
1.5 floor = 1 "floor (round down)"
1.2 isNumber = true "is x a number"
0 isInteger = true "is x an integer"
3 positive = true "is x > 0"
(1 max: 2) = 2 "maximum"
(1 min: 2) = 1 "minimum"
3 negated = -3 "negation"
3 odd = true "oddness predicate"
Float pi closeTo: 3.1415926535898 "constant pi (Float pi)"
(2 raisedTo: 3) = 8 "i to the power of j"
5 reciprocal = 0.2 "1 / x"
(Float pi / 2) sin = 1 "sine"
9 sqrt = 3 "square root"
3 squared = 9 "x * x"
Float pi radiansToDegrees = 180 "radiansToDegrees"

'Message sends'
2 sqrt = 1.4142135623730951 "unary message"
(2 raisedTo: 10) = 1024 "keyword message*"
(194 * 9) = 1746 "binary message"
(Transcript show: (194 * 9) printString; cr) = Transcript "combination (chaining)"
(2 perform: #sqrt) = 2 sqrt "indirect message send"
[ 1 unimplementedMessage ] on: Error do: [ true ]
6561 sqrt sqrt sqrt = 3 "unary message sequence"

'String'
String isVariable "St-80 = true, Squeak = false"
'string' isLiteral "string literal"
'123' asInteger = 123 "parse integer"
'12345' asNumber = 12345 "parse integer or float"
'3.141' asNumber = 3.141 "parse integer or float"
(String newFrom: #($P $e $n)) = 'Pen'
(#($P $e $n) as: String) = 'Pen'
'string' < 'symbol' "less than"
'string' = 'string' "equal"
'symbol' > 'string' "greater than"
'string' copy = 'string' copy "copied strings are equal"
'string' asString = 'string'
'string' copy asString = 'string' copy
'string' == 'string' "string literals are identical"
'string' copy ~~ 'string' copy "copied strings are not identical"
#('string') = {'string'}
(String withAll: ($a to: $z)) = 'abcdefghijklmnopqrstuvwxyz'
($a to: $z) = 'abcdefghijklmnopqrstuvwxyz'
'string' hash = 'string' copy hash "strings and copies of strings hash equally"
'1796p-qR' asInteger = 1796
'-94' asInteger = -94
('fred the bear' copyWithout: $e) = 'frd th bar'
String new species = String new species
'{\' = '{\'
'string' includes: $i. "true"
('string' includes: nil) = false
('antidisestablishmentarianism' contractTo: 10) = 'anti...ism'
('string' at: 3) = $r
('string' basicAt: 3) = $r
'string with "comment" characters' size = 32
'string with "unbalanced" comment " characters' size = 45
'\"' size = 2
('\"' at: 1) = $\
'string' asArray = #($s $t $r $i $n $g)
('a/b/c' splitBy: '/') =  #('a' 'b' 'c')
('/a/b/c' splitBy: '/') =  #('' 'a' 'b' 'c')
('a/b/c/' splitBy: '/') =  #('a' 'b' 'c' '')

'UndefinedObject'
nil hash = nil copy hash
nil copy = nil
nil copy == nil
(nil ifNil: ['nil'] ifNotNil: ['notNil']) = 'nil'
(#() ifNil: ['nil'] ifNotNil: ['notNil']) = 'notNil'

'Class hierachy'
(nil isKindOf: Class) not
Integer superclass = Number "discover the superclass of a class"
Integer subclasses includes: SmallInteger
(ArrayedCollection subclasses includes: Symbol) not
ArrayedCollection allSubclasses includes: Symbol
Array allSuperclasses size >= 4 "get sequence of all superclasses of a class"
(Set withAllSuperclasses) size >= 4 "an ordered collection of a class and it's parents"
