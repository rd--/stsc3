'Arithmetic expressions'

6 + 3 == 9                                                  // addition
6 - 3 == 3                                                  // subtraction
6 * 3 == 18                                                 // multiplication
1 + 2 * 3 == 9                                              // evaluation always left to right (1 + 2) * 3
(5 / 3).class == Float                                      // division with fractional result
5.0 / 2.0 == 2.5                                            // division with float result
5.0.div(3.0) == 1                                           // integer divide
5 % 3 == 2                                                  // modulo
5.0 % 3.0 == 2.0                                            // float modulo
-5 == 5.neg                                                 // unary minus
5.sign == 1                                                 // numeric sign (1, -1 or 0)
5.neg == -5                                                 // negate receiver
1.25.trunc.asInteger == 1                                   // integer part of number
1.25.frac == 0.25                                           // fractional part
5.reciprocal == (1/5)                                       // reciprocal function
6 * 3.1 == 18.6                                             // auto convert to float
5.squared == 25                                             // square function
25.sqrt == 5.0                                              // square root
5.0 ** 2.0 == 25.0                                          // power function
5 ** 2 == 25                                                // power function with integer
1.exp == 2.718281828459                                     // exponential
-5.abs == 5                                                 // absolute value
3.99.round == 4                                             // round
3.99.trunc == 3                                             // truncate
3.99.round(1) == 4.0                                        // round to specified decimal places"
3.99.trunc(1) == 3.0                                        // truncate to specified decimal places"
3.99.floor == 3                                             // round down
3.99.ceil == 4                                              // round up
5.factorial == 120                                          // factorial
28.gcd(12) == 4                                             // greatest common denominator
28.lcm(12) == 84                                            // least common multiple
1.0.exp.log == 1.0                                          // natural logarithm
100.log10 == 2.0                                            // base 10 logarithm
180.degrad == pi                                            // convert degrees to radians
pi.raddeg == 180.0                                          // convert radians to degrees
(pi / 2).sin == 1.0                                         // sine
0.0.cos == 1.0                                              // cosine
0.0.tan == 0.0                                              // tangent
10.max(20) == 20                                            // get maximum of two numbers
10.min(20) == 10                                            // get minimum of two numbers
pi == 3.141592653589793                                     // pi
1.exp                                                       // e
100.rand                                                    // random number in (0, self-1)

'Array'

Array.new(4) == []                                          // new array
Array.newClear(4) == [nil, nil, nil, nil]                   // new array with slots set to nil
#['x', 'y', 'z'] == ['x', 'y', 'z']                         // literal array syntax
#[1, 2, 3, 4] == [1, 2, 3, 4]                               // literal array syntax
[1, 2, 3, 4].at(0) == 1                                     // indexing (access element in slot at index)
[1, 2, 3, 4].collect({arg x; x * x}) == [1, 4, 9, 16]       // map function over elements
[1, 2, 3, 4].detect({arg x; x > 2}) == 3                    // find first element for which predicate holds
[1, 2, 3, 4].detectIndex({arg x; x > 2}) == 2               // find index of first element for which predicate holds
[1, 2, 3, 4].do({arg x; x.postln}) == [1, 2, 3, 4]          // run function at each element
[1, 2, 3, 4].inject(0, {arg z, e; e - z}) == 2              // left fold: (1 - 0)
[1, 2, 3, 4].injectr(0, {arg z, e; e - z}) == -2            // right fold
[].isEmpty == true                                          // Array -> Bool
[1, 2, 3, 4].includes(3) == true                            // has element
[1, 2, 3, 4].put(0, '1') == ['1', 2, 3, 4]                  // set element at index
[1, 2, 3, 4].reject({arg x; x > 2}) == [1, 2]               // filter elements
[1, 2, 3, 4].select({arg x; x > 2}) == [3, 4]               // filter elements
[1, 2, 3, 4].scramble                                       // randomly shuffle elements
[1, 2, 3, 4].size == 4                                      // array length
[1, 2, 3, 4].clump(2) == [[1, 2], [3, 4]]                   // collect subarrays of indicated size

'Assignment'

{var x, y, z; nil}                                          // variable declaration
{var x; x = 5; x == 5}                                      // assignment (set variable x to 5)
{var x, y, z; x = y = z = 5; [x, y, z] == [5, 5, 5]}        // compound assignment (set variables x y and z all to 5)
{var x, y; x = (y = 5) + 1; [x, y] == [6, 5]}               // expression assignment

'Association'

('x' -> 1) == Association.new('x', 1)                       // literal syntax
('x' -> 1).key == 'x'                                       // key (left hand side) of association
('x' -> 1).value == 1                                       // value (right hand side) of association

'Bitwise Manipulation'

0xFF.bitAnd(0x0F) == 0x0F                                   // and bits
0xF0.bitOr(0x0F) == 0xFF                                    // or bits
0xFF.bitXor(0x0F) == 0xF0                                   // xor bits
0xFF.bitNot == -0x100                                       // invert bits
0x0F.leftShift(4) == 0xF0                                   // left shift
0xF0.rightShift(4) == 0x0F                                  // right shift
0x80.bitTest(7) == true                                     // bit at position (0|1) [!Squeak]

'Booleans'

true == true                                                // constant
false == false                                              // constant
true.if({'ifTrue'.postln})                                  // if then
false.if({'ifTrue'.postln}, {'ifFalse'.postln})             // if then else (do)
true.if({'ifTrue'}, {'ifFalse'}).postln                     // if then else (value)
true.not.not == true                                        // not
1.isKindOf(Number) == true                                  // test if object is class or subclass of
1.isMemberOf(Integer) == true                               // test if object is type of class
1.respondsTo('sqrt') == true                                // test if object responds to message
nil.isNil == true                                           // test if object is nil
1.isPositive == true                                        // test if number is positive
0.isStrictlyPositive == false                               // test if number is greater than zero
-1.isNegative == true                                       // test if number is negative
2.even == true                                              // test if number is even
1.odd == true                                               // test if number is odd
1.isInteger == true                                         // test if object is integer
1.0.isFloat == true                                         // test if object is float
1.0.isNumber == true                                        // test if object is number
$A.isUpper == true                                          // test if upper case character
$a.isLower == true                                          // test if lower case character

'Character'

$x.class == Char
$x.ascii == 120                                             // Char -> Int
$x.asString == "x"                                          // Char -> String
$x.asSymbol == 'x'                                          // Char -> Symbol
$3.digit == 3                                               // Char -> Int
$x.isAlpha == true                                          // is alphabetic
$1.isDecDigit== true                                        // is [0-9]
$l.isLower == true                                          // is lower case
$ .isSpace == true                                          // is whitespace
$U.isUpper == true                                          // is upper case

'Class'

String.name == 'String'                                     // class name
String.instVarNames                                         // immediate instance variable names
String.classVarNames                                        // immediate class variable names
String.methods.inspect                                      // methods for class
String.methods.collect({ arg m; m.name })                   // message selectors for class
String.superclass == RawArray                               // immediate superclass
String.subclasses == nil                                    // immediate subclasses
String.allSubclasses == nil                                 // accumulated subclasses
Env.allSubclasses == [Penv]                                 // subclasses of class
1.class == Integer                                          // class of value
1.isKindOf(Number) == true                                  // is an instance of a class or a subclass
1.respondsTo('exp') == true                                 // responds to message

'Comments'

"comment"                                                   // until end of line comment
"comment"                                                   // /* multi-line, interior-line comment */

'Conditional Statements'

(9.rand > 4).if({ "ifTrue".postln })                        // if then
(9.rand > 4).if({ "ifTrue".postln }, { "ifFalse".postln })  // if then else
(9.rand > 4).if("ifTrue", "ifFalse").postln                 // if then else
2.rand.switch(0, { "A".postln }, 1, { "B".postln })         // switch

'Conversion'

1.0.asInteger == 1                                          // convert number to integer (truncates in Squeak)
1.5.asFraction == [3, 2]                                    // convert number to fraction
3.asFloat == 3.0                                            // convert number to float
97.asAscii == $a                                            // convert integer to character
$a.ascii == 97                                              // convert character to integer
$a.asString == "a"                                          // convert object to string
$a.asCompileString == "$a"                                  // convert object to string via storeOn:
15.asHexString(2) == "0F"                                   // convert to string in hex base

'Date'

Date.getDate                                                // current date and time
Date.localtime                                              // current local date and time
Date.localtime.rawSeconds.isFloat == true                   // fractional seconds
Date.localtime.stamp.isString == true                       // timestamp as string
Date.new(2020, 04, 12, 14, 57, 10, 1, nil).asString         // Mon Apr 12 14:57:10 2020

'Dictionary'

d = Dictionary.new(4)                                       // new empty dictionary
d.add('x' -> 1).add('y' -> 2).add('z' -> 3)                 // add association to dictionary
d == Dictionary.newFrom(['x', 1, 'y', 2, 'z', 3])           // new Dictionary from sequence
d.associationsDo({arg x; x.postln})                         // run f at associations of dictionary
d.at('x') == 1                                              // lookup value at key
d.at('_') == nil                                            // lookup value at key, nil if key not present
d.do({arg x; x.postln})                                     // run f at values of dictionary
d.includes(3) == true                                       // does dictionary have indicated value
d.includesKey('z') == true                                  // does dictionary have indicated key
d.keys == Set.newFrom(['x', 'y', 'z'])                      // set of keys at dictionary
d.keysDo({arg x; x.postln})                                 // run f at keys of dictionary
d.keysValuesDo({arg k, v; [k, v].postln})                   // run f at keys & values of dictionary
d.occurrencesOf(3) == 1                                     // number of occurences of value in dictionary
{d.removeAt('x') == 1; d.add('x' -> 1)}.value               // remove (and return value associated with) key from dictionary
d.size == 3                                                 // size of dictionary (number of associations)
d.values.sort({arg i, j; i <= j}) == List.newFrom([1, 2, 3])// list of values at dictionary

'Interval'

Interval.new(0, 12, 3).asArray == [0, 3, 6, 9, 12]          // elements of interval as array
Interval.new( 0, 12, 3).size == 5                           // number of elements in interval
2.to(14, 4).collect({arg x; x * x}) == [4, 36, 100, 196]    // to method at Integer

'Iteration'

4.do({ 'x'.postln}) == 4                                    // times repeat loop (int)
4.do({ arg x; x.postln }) == 4                              // for loop (int)
1.to(5).do({ arg x; x.postln }) == 1.to(5)                  // for loop (interval)
[1, 2, 3, 4, 5].do({ arg x; x.postln }) == [1, 2, 3, 4, 5]  // for loop (collection)
1.for(5, { arg x; x.postln }) == 1                          // for loop
{ 9.rand > 3 }.while({ 'x'.postln }) == nil                 // while loop

'Magnitude'

1 < 3 == true                                               // less than
3 <= 3 == true                                              // less than or equal to
3 > 1 == true                                               // greater than
1 >= 1 == true                                              // greater than or equal to
2 == 2 == true                                              // equal to

'Math'

-3.abs == 3                                                 // absolute value
1.5.ceil== 2                                                // ceiling (round up)
0.cos == 1                                                  // cosine
180.degrad == pi                                            // degreesToRadians
2.even == true                                              // eveness predicate
1.exp == 2.718281828459045                                  // base e exponent function
1.5.floor == 1                                              // floor (round down)
1.exp.log == 1                                              // base e (natural) logarithm
1.2.isFloat == true                                         // is x a float
0.isInteger == true                                         // is x an integer
3.isPositive == true                                        // is x > 0
1.max(2) == 2                                               // maximum
1.min(2) == 1                                               // minimum
3.neg == -3                                                 // negation
3.odd == true                                               // oddness predicate
pi == 3.1415926535898                                       // constant pi (Float pi)
2.pow(3) == 8                                               // i to the power of j
5.reciprocal == 0.2                                         // 1 / x
(pi/2).sin == 1                                             // sine
9.sqrt == 3                                                 // square root
3.squared == 9                                              // x * x
pi.raddeg == 180                                            // radiansToDegrees

'Method'

9.sqrt == 3                                                 // unary method
1.min(2) == 1                                               // binary "keyword" method
1 + 2 == 3                                                  // binary "operator" method
9.perform('sqrt') == 3                                      // indirect method invocation (unary)
1.perform('min', 2) == 1                                    // indirect method invocation (binary)
1.perform('+', 2) == 3                                      // indirect method invocation (operator)
1.exp.log == 1                                              // method chaining (log of exp of 1)

'Point'

200@100                                                     // obtain a new point
(200@100).x                                                 // x coordinate
(200@100).y                                                 // y coordinate
0 - (200@100)                                               // negates x and y
(0 - (200@100)).abs                                         // absolute value of x and y
200@100 + 1                                                 // add scale to both x and y
200@100 - 1                                                 // subtract scale from both x and y
200@100 * 2                                                 // multiply x and y by scale
200@100 / 2                                                 // divide x and y by scale
(200@100).div(2)                                            // divide x and y by scale
200@100 % 3                                                 // modulo of x and y by scale
200@100 + (50@25)                                           // add points
200@100 - (50@25)                                           // subtract points
200@100 * (3@4)                                             // multiply points
200@100 / (3@4)                                             // divide points

'Post'

"text".postln                                               // write text to post window (console)
'sym'.postln                                                // write object to post window (console)

'SequenceableCollection'

[1, 3, 2, 4, 5].sort(nil) == [1, 2, 3, 4, 5]                // sort using default comparison
[1, 3, 2, 4, 5].sort({arg i, j; i <= j}) == [1, 2, 3, 4, 5] // sort using provided comparison

'String'

"quoted string"                                             // quoted string
"str" ++ "ing" == "string"                                  // catenation
"one" + "two" == "one two"                                  // catenation with space
"/usr" +/+ "local" == "/usr/local"                          // file path catenation
"string".ascii == [115, 116, 114, 105, 110, 103]            // String -> [Int]
"3.4".asFloat == 3.4                                        // parse float
"3".asInteger == 3                                          // parse integer
"string".asSymbol == 'string'                               // String -> Symbol
"string".at(4) == $n                                        // indexing
"string".isEmpty == false                                   // is empty string
"".isEmpty == true                                          // empty string
"string".mutable == false                                   // string literals are immutable
'string'.asString.put(3, $u) == "strung"                    // mutate string setting char at indexed slot
'string'.asString.removeAt(5) == $g                         // mutate string removing char at slot
"string".size == 6                                          // length
"str ing".split($ ) == ["str", "ing"]                       // split at char
"a" < "b" == true                                           // string comparison
"dcba".copy.sort == "abcd"                                  // inplace string sort

'Symbol'

'symbol'.asString == "symbol"                               // Symbol -> String
'a' ++ 'b' == "ab"                                          // Symbol -> Symbol -> String, c.f. (#a , #b) = 'ab'
'a' < 'b' == true                                           // symbol ordering

'Debugging'

String.browse                                               // browse specified class
Server.default.inspect                                      // open object inspector window
nil.halt                                                    // breakpoint to stop interpreter
"Error string".error                                        // print error string
"Error string".reportError                                  // print error string and print backtrace
nil.doesNotUnderstand('unknownMessage')                     // flag message is not handled
nil.shouldNotImplement                                      // flag message should not be implemented
nil.subclassResponsibility                                  // flag message as abstract
nil.primitiveFailed                                         // system primitive failed
