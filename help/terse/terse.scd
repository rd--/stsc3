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
pi == 3.141592653589793                                     // pi (3.141592653589793)
1.exp                                                       // e (2.718281828459)
100.rand                                                    // random number in (0, self-1)
4 + 5 * 6 == 54                                             // operators are evaluated left to right

'Assignment'

var x = 5;                                                  // assignment with initializer expression
x = 5                                                       // assignment
x = y = z = 6                                               // compound assignment
x = (y = 6) + 1                                             // compound assignment
x = 1.2.hash                                                // hash value for object
y = x.copy                                                  // copy object
y = x.shallowCopy                                           // copy object (not overridden)
y = x.deepCopy                                              // copy object and instance vars

'Array'

Array.new.isKindOf(SequenceableCollection) == true          // arrays are sequenceable collections
Array.new(4) == []                                          // new empty array
Array.new(4).size == 0                                      // new empty array
Array.newClear(4) == [nil, nil, nil, nil]                   // new array with slots set to nil
Array.fill(3, { 1 }) == [1, 1, 1]                           // new array with slots set by calling function
#['x', 'y', 'z'] == ['x', 'y', 'z']                         // literal array syntax
#[1, 2, 3, 4] == [1, 2, 3, 4]                               // literal array syntax
#[]                                                         // empty literal array
#[1, 3.14, 'x', \x, "x"]                                    // heterogenous literal array
[1, 2, 3, 4].at(0) == 1                                     // indexing (access element in slot at index)
[1, 2, 3, 4].collect({ arg x; x * x }) == [1, 4, 9, 16]     // map function over elements
[1, 2, 3, 4].detect({ arg x; x > 2 }) == 3                  // find first element for which predicate holds
[1, 2, 3, 4].detectIndex({ arg x; x > 2 }) == 2             // find index of first element for which predicate holds
[1, 2, 3, 4].do({ arg x; x.postln }) == [1, 2, 3, 4]        // iterate over array (run function at each element)
[1, 2, 3, 4].inject(0, { arg z, e; e - z }) == 2            // left fold: (1 - 0)
[1, 2, 3, 4].injectr(0, { arg z, e; e - z }) == -2          // right fold
[].isEmpty == true                                          // is array empty
[1, 2, 3, 4].includes(3) == true                            // test if element is in array
[1, 2, 3, 4].copyRange(1, 2) == [2, 3]                      // subarray
[1, 2, 3, 4].every({ arg x; x < 5 }) == true                // test if all elements meet condition
[1, 2, 3, 4].any({ arg x; x == 3 }) == true                 // test if any elements meet condition
[1, 2, 3, 4].includes(3) == true                            // has element
[1, 2, 3, 4].put(0, '1') == ['1', 2, 3, 4]                  // set element at index
[1, 2, 3, 4].reject({ arg x; x > 2 }) == [1, 2]             // filter elements
[1, 2, 3, 4].select({ arg x; x > 2 }) == [3, 4]             // filter elements
[1, 2, 3, 4].scramble                                       // randomly shuffle elements
[1, 2, 3, 4].size == 4                                      // array length
[1, 2, 3, 4].clump(2) == [[1, 2], [3, 4]]                   // collect subarrays of indicated size
{ 1 }.dup(3) == [1, 1, 1]                                   // Array.fill notation
{ 1 } ! 3 == [1, 1, 1]                                      // infix Array.fill notation
Array.series(4, 3, 2) == [3, 5, 7, 9]                       // arithmetic series (size from by)
Array.geom(4, 3, 2) == [3, 6, 12, 24]                       // geometric series (size from by)
Array.iota(4) == [0, 1, 2, 3]                               // array with counter
Array.iota(3, 2) == [[0, 1], [2, 3], [4, 5]]                // matrix with counter
Array.rand(4, 1, 9)                                         // array of random numbers (uniform distribution)
Array.linrand(4, 1, 9)                                      // array of random numbers (linear distribution)
Array.exprand(4, 1, 9)                                      // array of random numbers (exponential distribution)
3.series(5, 9) == [3, 5, 7, 9]                              // arithmetic series (from then to)

'Assignment'

{ var x, y, z; nil }                                        // variable declaration
{ var x; x = 5; x == 5 }                                    // assignment (set variable x to 5)
{ var x, y, z; x = y = z = 5; [x, y, z] == [5, 5, 5] }      // compound assignment (set variables x y and z all to 5)
{ var x, y; x = (y = 5) + 1; [x, y] == [6, 5] }             // expression assignment

'Association'

Association.new('x', 1)                                     // associate key with value
'x' -> 1 == Association.new('x', 1)                         // literal syntax
('x' -> 1).key == 'x'                                       // key (left hand side) of association
('x' -> 1).value == 1                                       // value (right hand side) of association

'Bitwise Manipulation'

2.bitAnd(3) == 2                                            // and bits
2.bitOr(3) == 3                                             // or bits
2.bitXor(3) == 1                                            // xor bits
3.bitNot == -4                                              // invert bits
2.leftShift(3) == 16                                        // left shift
16.rightShift(3) == 2                                       // right shift
2.bitTest(1) == true                                        // bit at position (0|1) [!Squeak]
2 & 3 == 2                                                  // and bits operator
2 | 3 == 3                                                  // or bits operator
2 << 3 == 16                                                // left shift operator
16 >> 3 == 2                                                // right shift operator

'Booleans'

true == true                                                // constant
false == false                                              // constant
1 == 1 == true                                              // equality predicate (operator)
1 != 2 == true                                              // equality predicate (operator)
true.and(false) == false                                    // logical and
true.or(false) == true                                      // logical or
true && false == false                                      // logical and (operator)
true || false == true                                       // logical or (operator)
true.if({ 'ifTrue'.postln })                                // if then, c.f. conditional statements
false.if({ 'ifTrue'.postln }, { 'ifFalse'.postln })         // if then else (do)
true.if({ 'ifTrue' }, { 'ifFalse' }).postln                 // if then else (value)
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
#[$., $$, $ ]                                               // notable characters (period, dollar, space)

'Class'

String.isKindOf(Class) == true                              // class
String.name == 'String'                                     // class name
String.class == Meta_String                                 // meta class (class of a class)
String.class.class == Class                                 // class of a meta class is Class
String.instVarNames                                         // immediate instance variable names
String.classVarNames                                        // immediate class variable names
String.methods.inspect                                      // methods for class
String.methods.collect({ arg m; m.name })                   // message selectors for class
String.superclass == RawArray                               // immediate superclass
String.superclasses.size == 5                               // all superclass
UGen.superclasses == [AbstractFunction, Object]             // superclass sequence
UGen.subclasses.size == 362                                 // immediate subclasses
UGen.allSubclasses.size == 884                              // accumulated subclasses
1.class == Integer                                          // class of value
1.isKindOf(Number) == true                                  // is instance of a class or a subclass
1.isMemberOf(Integer) == true                               // is instance of a class
1.respondsTo('exp') == true                                 // responds to message

'Collection'

Collection.allSubclasses.size == 41                         // collection class
[].isEmpty == true                                          // is collection empty
[1, 2, 3] == [1, 2, 3]                                      // are collections equal
[9, 4, 5, 7, 8, 6].size == 6                                // size of collection
[9, 4, 5, 7, 8, 6].maxItem == 9                             // maximum item in collection
[9, 4, 5, 7, 8, 6].minItem == 4                             // minimum item in collection
[9, 4, 5, 7, 8, 6].sum == 39                                // sum of collection (inject 0 +)
[9, 4, 5, 7, 8, 6].mean == 6.5                              // sum of collection divided by size
[9, 4, 5, 7, 8, 6].product == 60480                         // product of collection (inject 1 *)
[9, 4, 5, 7, 8, 6].inject(0, { arg z, e; e + z }) == 39     // sum of collection
[9, 4, 5, 7, 8, 6].inject(1, { arg z, e; e * z }) == 60480  // product of collection
[9, 4, 5, 7, 8, 6].includes(7) == true                      // is element in collection
[9, 4, 5, 7, 8, 6].count({ arg item; item.even }) == 3      // count elements that satisfy predicate
[9, 4, 5, 7, 8, 6].any({ arg item; item.even }) == true     // do any elements satisfy predicate
[9, 4, 5, 7, 8, 6].every({ arg item; item.even }) == false  // do all elements satisfy predicate
[9, 4, 5, 7, 8, 6].occurrencesOf(7) == 1                    // count elements that are equal to object
[1, 2, 3].union([3, 4, 5]) == [1, 2, 3, 4, 5]               // combine two collections
[1, 2, 3].isSubsetOf([1, 2, 3, 4, 5]) == true               // is one collection a subset of the other
[1, [2, 3], [4, [5, 6]]].maxDepth == 3                      // tree depth of collection
[1, [2, 3], [4, [5, 6]]].deepCollect(3, { arg x; x -> '.'}) // tree collect to indicated depth
[1, 2, 3, 4, 5].choose                                      // random element of collection
[1, 2, 3, 4, 5].wchoose([1, 2, 3, 4, 5] / 15)               // weighted random element of collection (weights sum to 1)

'Comments'

"until end of line comment"                                 // until end of line comment
"multi-line or interior-line comment"                       // /* multi-line or interior-line comment */

'Complex'

Complex.new(-1, 0).real == -1                              // real part of complex number
Complex.new(-1, 0).imag == 0                               // imaginary part of complex number
Complex.new(-1, 0) + 1                                     // complex addition with scalar
Complex.new(-1, 0).pow(1 / 2)                              // complex math

'Conditional Statements'

(9.rand > 4).if({ 'ifTrue'.postln })                        // if then
(9.rand > 4).if({ 'ifTrue'.postln }, { 'ifFalse'.postln })  // if then else
(9.rand > 4).if('ifTrue', 'ifFalse').postln                 // if then else
2.rand.switch(0, { 'A'.postln }, 1, { 'B'.postln })         // switch

'Constants'

true                                                        // true constant
false                                                       // false constant
nil                                                         // nil object constant
inf.class == Float                                          // Infinity constant (instance of Float)
pi.class == Float                                           // pi constant (instance of Float)
1                                                           // integer constants
3.14                                                        // float constants
-1                                                          // negative integer constants
-3.14                                                       // negative float constants
"Hello"                                                     // string constant (\ quotes quote)
$a                                                          // character constant
$                                                           // character constant (space)
'aSymbol'                                                   // symbol constants
'a Symbol'                                                  // symbol constants (space)
\aSymbol                                                    // prefix symbol notation
#[3, 2, 1]                                                  // array constants
#['a', "a", $a, 1]                                          // mixing of types allowed

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
d.associationsDo({ arg x; x.postln })                       // run f at associations of dictionary
d.at('x') == 1                                              // lookup value at key
d.at('_') == nil                                            // lookup value at key, nil if key not present
d.do({ arg x; x.postln })                                   // run f at values of dictionary
d.includes(3) == true                                       // does dictionary have indicated value
d.includesKey('z') == true                                  // does dictionary have indicated key
d.keys == Set.newFrom(['x', 'y', 'z'])                      // set of keys at dictionary
d.keysDo({ arg x; x.postln })                               // run f at keys of dictionary
d.keysValuesDo({ arg k, v; [k, v].postln })                 // run f at keys & values of dictionary
d.occurrencesOf(3) == 1                                     // number of occurences of value in dictionary
{ d.removeAt('x') == 1; d.add('x' -> 1) }.value             // remove (and return value associated with) key from dictionary
d.size == 3                                                 // size of dictionary (number of associations)
d.values.sort({ arg i, j; i <= j })                         // list of values at dictionary

'Interpreter'

this.class == Interpreter                                   // context in which interactive commands
this.interpret("1 + 2") == 3                                // interpret string
this.interpret("{ arg x; x + 2 }").value(1) == 3            // interpret string
this.s.class == Server                                      // by convention the Interpreter variable s has the default scsynth
this.executeFile("program.scd")                             // run program text

'Interval'

Interval.new(0, 12, 3).asArray == [0, 3, 6, 9, 12]          // elements of interval as array
Interval.new( 0, 12, 3).size == 5                           // number of elements in interval
2.to(14, 4).collect({ arg x; x * x }) == [4, 36, 100, 196]  // to method at Integer

'Iteration'

4.do({ 'x'.postln}) == 4                                    // times repeat loop (int)
4.do({ arg x; x.postln }) == 4                              // for loop (int)
4.reverseDo({ arg x; x.postln }) == 4                       // for loop (int, reversed)
1.to(5).do({ arg x; x.postln }) == 1.to(5)                  // for loop (interval)
[1, 2, 3, 4, 5].do({ arg x; x.postln }) == [1, 2, 3, 4, 5]  // for loop (collection)
1.for(5, { arg x; x.postln }) == 1                          // for loop
1.forBy(9, 2, { arg x; x.postln }) == 1                     // for by loop
{ 9.rand > 3 }.while({ 'x'.postln }) == nil                 // while loop

'Files'

File.open("/tmp/x", "w").put('x').close                     // open file, put value, close file
File.use("/tmp/x", "w", { arg f; f.put('x') })              // use file to put value
File.exists("/tmp/x") == true                               // file exists predicate
File.fileSize("/tmp/x") == 1                                // file size (bytes)
File.readAllString("/tmp/x") == "x"                         // read file contents (string)
File.use("/tmp/x", "r", { arg f; f.getChar }) == $x         // use file to read character
File.delete("/tmp/x")                                       // delete file

'Functions (Blocks)'

x = { }                                                     // blocks are objects and may be assigned to a variable
{ nil; 1 }.value == 1                                       // value is last expression evaluated unless explicit return
{ { 1 }.value }.value == 1                                  // blocks may be nested
{ arg x; var y = x; y }.value(1) == 1                       // specification { arguments localvars expressions }
{ ^1 }.value                                                // ^expression terminates block & method (exits all nested blocks)
{ 'x'.postln }.value                                        // simple block usage
{ arg p1, p2; p1 ++ ' and ' ++ p2 }.value('x', 'y')         // block with argument passing
{ arg x; x + 1 }.def.sourceCode == "{ arg x; x + 1 }"       // the FunctionDef can be retrieved
({ arg x; x * 2 } <> { arg x; x + 3 }).value(4) == 14       // function composition

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
inf                                                         // constant positive infinity
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
1.exp.log == 1                                              // unary method chaining (log of exp of 1)
1.min(2).max(3) == 3                                        // n-ary method chaining

'Nil'

nil.class == Nil                                            // nil
nil ? 'x' == 'x'                                            // right hand side if nil
'x' ? 'y' == 'x'                                            // left hand side unless nil
nil.isNil == true                                           // is nil

'Point'

200 @ 100 == Point.new(200, 100)                            // obtain a new point
(200 @ 100).x                                               // x coordinate
(200 @ 100).y                                               // y coordinate
0 - (200 @ 100)                                             // negates x and y
(0 - (200 @ 100)).abs                                       // absolute value of x and y
200 @ 100 + 1                                               // add scale to both x and y
200 @ 100 - 1                                               // subtract scale from both x and y
200 @ 100 * 2                                               // multiply x and y by scale
200 @ 100 / 2                                               // divide x and y by scale
(200 @ 100).div(2)                                          // divide x and y by scale
200 @ 100 % 3                                               // modulo of x and y by scale
200 @ 100 + (50 @ 25)                                       // add points
200 @ 100 - (50 @ 25)                                       // subtract points
200 @ 100 * (3 @ 4)                                         // multiply points
200 @ 100 / (3 @ 4)                                         // divide points

'Post'

"text".postln                                               // write text to post window (console)
'sym'.postln                                                // write object to post window (console)

'Pseudo variables'

false.class == False                                        // constant (instance of False)
nil.class == Nil                                            // constant (instance of Nil)
pi.class == Float                                           // constant (instance of Float)
inf.class == Float                                          // constant (infinity, instance of Float)
super.class == Interpreter                                  // super (self starting name lookup at parent)
this.class == Interpreter                                   // self
thisFunction.class == Function                              // context
thisFunctionDef.class == FunctionDef                        // context
thisMethod.class == Method                                  // context
thisProcess.class == Main                                   // context
thisThread.class == Thread                                  // context
true.class == True                                          // constant (instance of True)

'Random values'

{ 9.rand }.dup(9)                                           // random integers (0 to self)
{ 0.9.rand }.dup(3)                                         // random floats (0 to self)
#[1, 2, 3, 4, 5].choose                                     // random element of collection
#[1, 2, 3, 4, 5].wchoose(#[1, 2, 3, 4, 5] / 15)             // weighted random element of collection (weights sum to 1)
{ 3.rrand(9) }.dup(9)                                       // random integer in range
{ 0.3.rrand(0.9) }.dup(3)                                   // random float in range

'Routine'

f = { arg x; var y = x * 2; var z = y.yield; f.value(z) }   // yield send the receiver to the coroutine (it is received by next)
r = Routine.new(f)                                          // a routine is instantiated on a function
1.to(9).collect({ arg i; var j = r.next(i); i -> j })       // next sends a value to a routine (resume, value, and run are synonyms)

'SequenceableCollection'

SequenceableCollection.allSubclasses.size == 180            // sequencable collection class
[1, 3, 2] ++ [4, 5] == [1, 3, 2, 4, 5]                      // append sequences
[1, 3, 2, 4, 5].sort == [1, 2, 3, 4, 5]                     // sort using default comparison
[1, 3, 2, 4, 5].sort({ arg i, j; i <= j })                  // sort using provided comparison
[1, 2, 3, 4, 5].keep(3) == [1, 2, 3]                        // keep first n items
[1, 2, 3, 4, 5].drop(2) == [3, 4, 5]                        // discard first n items
[1, 3, 5, 7, 9].isSeries == true                            // is array an arithmetic series
[1, 3, 5, 7, 9].resamp0(9) == [1, 3, 3, 5, 5, 7, 7, 9, 9]   // resampler
[1, 3, 5, 7, 9].resamp1(9) == [1, 2, 3, 4, 5, 6, 7, 8, 9]   // linear interpolating resampler
[1, 3, 5, 7, 9].copyRange(2, 4) == [5, 7, 9]                // copy part of collection
[1, 3, 5, 7, 9].find([5, 7, 9]) == 2                        // locate index of subsequence
[1, 3, 5, 7, 9].indexOf(5) == 2                             // index of element (compared using ==)
[1, 3, 5, 7, 9].first == 1                                  // first element of
[1, 3, 5, 7, 9].last == 9                                   // last element of
[1, 3, 5, 7, 9].middle == 5                                 // middle element of
[1, 3, 5, 7, 9].clump(3) == [[1, 3, 5], [7, 9]]             // groups of n elements
[1, 3, 5, 7, 9].clumps([2, 1]) == [[1, 3], [5], [7, 9]]     // groups of n elements
[1, 3, 5, 7, 9].clump(2).flatten == [1, 3, 5, 7, 9]         // concatentate
[1, 3, 5, 7, 9].integrate == [1, 4, 9, 16, 25]              // incremental sum
[1, 4, 9, 16, 25].differentiate == [1, 3, 5, 7, 9]          // pairwise difference

'String'

"quoted string"                                             // quoted string
"x" ++ "y" == "xy"                                          // catenation
"x" + "y" == "x y"                                          // catenation with space
"/usr" +/+ "local" == "/usr/local"                          // file path catenation
"string".ascii == [115, 116, 114, 105, 110, 103]            // String -> [Int]
"3.4".asFloat == 3.4                                        // parse float
"3".asInteger == 3                                          // parse integer
"string".asSymbol == 'string'                               // String -> Symbol
"string".at(4) == $n                                        // indexing
"".isEmpty == true                                          // empty string predicate
"string".isEmpty == false                                   // is empty string
"string".mutable == false                                   // string literals are immutable
'string'.asString.put(3, $u) == "strung"                    // mutate string setting char at indexed slot
'string'.asString.removeAt(5) == $g                         // mutate string removing char at slot
"string".size == 6                                          // length
"str ing".split($ ) == ["str", "ing"]                       // split at char
"a" < "b" == true                                           // string comparison
"dcba".copy.sort == "abcd"                                  // inplace string sort
"text".copyRange(1,2) == "ex"                               // substring
"text".copyRange(2,2) == "x"                                // substring (single character)

'Symbol'

'symbol'.class == Symbol                                    // symbol
Symbol.superclass == Object                                 // Symbol is not a subclass of String
'symbol'.asString == "symbol"                               // convert to string
'x y'.asCompileString == "'x y'"                            // convert to quoted string
'a' ++ 'b' == "ab"                                          // Symbol -> Symbol -> String, c.f. (#a , #b) = 'ab'
'a' < 'b' == true                                           // symbol ordering

'Temporaries'

{ var x; x }.value == nil                                   // uninitialised variables are nil
{ var x = 1, y = 2; [x, y] }.value == [1, 2]                // var can introduce multiple temporaries
{ var x; var y = 0, z; [x, y, z] }.value == [nil, 0, nil]   // there can be multiple var sequences

'Debugging'

String.browse                                               // browse specified class
Server.default.inspect                                      // open object inspector window
nil.halt                                                    // breakpoint to stop interpreter
"Text".error                                                // print error string
"Text".reportError                                          // print error string and print backtrace
nil.doesNotUnderstand('unknownMessage')                     // flag message is not handled
nil.shouldNotImplement                                      // flag message should not be implemented
nil.subclassResponsibility                                  // flag message as abstract
nil.primitiveFailed                                         // system primitive failed
thisProcess.recompile                                       // recompile the class library
