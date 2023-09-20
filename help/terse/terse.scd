'A Terse Guide to SuperCollider - c.f. http://wiki.squeak.org/squeak/5699 by Chris Rathman'

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
[-5, 0, 5].collect { | each | each.sign } == [-1, 0, 1]     // numeric sign (1, -1 or 0)
5.neg == -5                                                 // negate receiver
1.25.trunc.asInteger == 1                                   // integer part of number
-1.25.trunc.asInteger == -2                                 // except for negative numbers
3.1.roundUp == 4                                            // round upwards (away from zero for positive)
-3.1.roundUp == -3                                          // round upwards (towards zero for negative)
1.25.frac == 0.25                                           // fractional part
-2.25.frac == 0.75                                          // except for negative numbers
5.reciprocal == (1/5)                                       // reciprocal function
6 * 3.1 == 18.6                                             // auto convert to float
5.squared == 25                                             // square function
25.sqrt == 5.0                                              // square root
5.0 ** 2.0 == 25.0                                          // power function
5 ** 2 == 25                                                // power function with integer
1.exp - 2.718281828459 < 0.00001                            // exponential
-5.abs == 5                                                 // absolute value
3.99.round == 4                                             // round ; c.f. rounded
3.99.trunc == 3                                             // truncate
3.99.round(1) == 4.0                                        // round to specified decimal places ; c.f. roundTo:
3.99.trunc(1) == 3.0                                        // truncate to specified decimal places
12345.trunc(600) == 12000                                   // truncate to integer
3.99.floor == 3                                             // round down
3.99.ceil == 4                                              // round up
5.factorial == 120                                          // factorial
18.doubleFactorial == 185794560.0                           // doubleFactorial
3.risingFactorial(9) == 19958400.0                          // risingFactorial
(3 .. 11).product == 19958400                               // product of interval
11.fallingFactorial(9) == 19958400.0                        // fallingFactorial
(11 .. 3).product == 19958400                               // product of interval
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
1.exp - 2.718281828459 < 0.00001                            // e (2.718281828459)
var n = 100.rand; (n >= 0) && { n < 100 }                   // random number in (0, self-1)
var n = 100.rand; n.inclusivelyBetween(0, 99)               // random number in (0, self-1)
var n = 98.rand + 1; n.exclusivelyBetween(0, 100)           // random number in (0, self-1)
4 + 5 * 6 == 54                                             // operators are evaluated left to right
300.nextPowerOfTwo == 512                                   // next power of two
300.previousPowerOf(2) == 256                               // previous power of two

'Array'
Array.new.isKindOf(Collection) == true                      // c.f. Collection
[].isCollection                                             // isCollection predicate
Array.new.isKindOf(SequenceableCollection) == true          // c.f. SequenceableCollection
[].isSequenceableCollection                                 // isSequenceableCollection predicate
Array.new.isKindOf(ArrayedCollection) == true               // c.f. ArrayedCollection
[].isArray                                                  // isArray predicate
Array.new(4) == []                                          // new empty array
Array.new(4).size == 0                                      // new empty array
Array.newClear(4) == [nil, nil, nil, nil]                   // new array with slots set to nil
Array.fill(3) { 1 } == [1, 1, 1]                            // new array with slots set by value protocol
Array.new(4).maxSize == 4                                   // Array has both size and maxSize
Array.new.add(1) == [1]                                     // add element to array, return array
Array.new(4).add(1).add(2).add(3) == [1, 2, 3]              // add elements to end of array
Array.new(3).add(1).addAll([2, 3]) == [1, 2, 3]             // add all elements of collection to end of array
var x = Array.new; x.addAll("string"); x.at(5) == nil       // ArrayedCollection is maybe-copying
var x = [1, 2]; [x.remove(2), x] == [2, [1]]                // remove element from array (in place)
[1, 2, 3, 4].removeAll([3, 4]) == [1, 2]                    // remove all elements of collection from array
#['x', 'y', 'z'] == ['x', 'y', 'z']                         // literal array syntax
#[1, 2, 3, 4] == [1, 2, 3, 4]                               // literal array syntax
#[].size == 0                                               // empty literal array
#[1, 3.14, 'x', \x, "x"].size == 5                          // heterogenous literal array
[1, 2, 3, 4].at(0) == 1                                     // indexing (access element in slot at index)
[1, 2, 3, 4].at(5) == nil                                   // out of bounds access answers nil
[1, 2, 3, 4].collect { | x | x * x } == [1, 4, 9, 16]       // Map function over elements
[1, 2, 3, 4].collect { | x | x * x } == [1, 4, 9, 16]       // map function over elements
[1, 2, 3, 4].detect { | x | x > 2 } == 3                    // find first element for which predicate holds
[1, 2, 3, 4].detectIndex { | x | x > 2 } == 2               // find index of first element for which predicate holds
var n = 0; [1, 2, 3, 4].do { | x | n = n + x }; n == 10     // iterate over array (run function at each element)
[1, 2, 3, 4].inject(0) { | z e | e - z } == 2               // left fold: (1 - 0)
[1, 2, 3, 4].injectr(0) { | z e | e - z } == -2             // right fold
[].isEmpty == true                                          // is array empty
[1, 2, 3, 4].includes(3) == true                            // test if element is in array (true)
(1 .. 7).includes(9) == false                               // test if element is in array (false)
(1 .. 9).includesAll((3 .. 7)) == true                      // has all elements of another collection
(1 .. 9).includesAny([0, 6]) == true                        // includes any element of a collection
[1, 2, 3, 4].copyRange(1, 2) == [2, 3]                      // subarray
[1, 2, 3, 4].every { | x | x < 5 } == true                  // test if all elements meet condition
[1, 2, 3, 4].any { | x | x == 3 } == true                   // test if any elements meet condition
[1, 2, 3, 4].put(0, '1') == ['1', 2, 3, 4]                  // set element at index
[1, 2, 3, 4].reject { | x | x > 2 } == [1, 2]               // filter elements
[1, 2, 3, 4].select { | x | x > 2 } == [3, 4]               // filter elements
[1, 2, 3, 4].scramble.sort == [1, 2, 3, 4]                  // randomly shuffle elements
var c = [1, 2, 3]; c.scramble; c == [1, 2, 3]               // scramble is non-mutating
(1 .. 4).mirror == [1, 2, 3, 4, 3, 2, 1]                    // append reverse of prefix of array
(1 .. 4).mirror1 == [1, 2, 3, 4, 3, 2]                      // mirror without last element
(1 .. 4).mirror2 == [1, 2, 3, 4, 4, 3, 2, 1]                // append reverse of array
[[1, 2], [3, 4], [5, 6]].lace == [1, 3, 5, 2, 4, 6]         // interleave elements from sub-arrays
[1, 3, 5, 2, 4, 6].unlace(3) == [[1, 2], [3, 4], [5, 6]]    // inverse of lace, argument tells number of subarrays
[1, 4, 2, 5, 3, 6].unlace(2) == [[1, 2, 3], [4, 5, 6]]      // inverse of lace, argument tells number of subarrays
[[1, 2], [3, 4], [5, 6]].lace == [1, 3, 5, 2, 4, 6]         // interleave elements from sub-arrays
[1, 2, 3, 4].size == 4                                      // array length
[1, 2, 3, 4].clump(2) == [[1, 2], [3, 4]]                   // collect subarrays of indicated size
{ 1 }.dup(3) == [1, 1, 1]                                   // Array.fill notation
{ 1 } ! 3 == [1, 1, 1]                                      // infix Array.fill notation
Array.series(4, 3, 2) == [3, 5, 7, 9]                       // arithmetic series (size from by)
3.series(5, 11) == [3, 5, 7, 9, 11]                         // arithmetic series (first second last)
5.geom(3, 2) == [3, 6, 12, 24, 48]                          // geometric series (size from by)
Array.iota(4) == [0, 1, 2, 3]                               // array with counter
Array.iota(3, 2) == [[0, 1], [2, 3], [4, 5]]                // matrix with counter
Array.rand(4, 1, 9).size == 4                               // array of random numbers (uniform distribution)
Array.linrand(4, 1, 9).size == 4                            // array of random numbers (linear distribution)
Array.exprand(4, 1, 9).size == 4                            // array of random numbers (exponential distribution)
3.series(5, 9) == [3, 5, 7, 9]                              // arithmetic series (from then to)
[1, 2, 3].reverse == [3, 2, 1]                              // reverse elements
var c = [1, 2]; c.reverse; c == [1, 2]                      // reverse is not in place
var x = [1, 2], y = x.reverse; x + y == [3, 3]              // reverse is not in place
[1, 3, 7, 9].rand.maxItem < 9                               // collect with .rand
[[1], [[2]], [[[3]]]].collect('rank') == [1, 2, 3]          // rank
[[1, 2, 3], [4, 5, 6]].shape == [2, 3]                      // size of shape is rank
[[1, 2], [3, 4], [5, 6]].shape == [3, 2]                    // size of each element is size at depth
[[[1], [2]], [[3], [4]], [[5], [6]]].shape == [3, 2, 1]     // rank and shape both assume regularity
[1, 2, 3, 4].pyramid(1) == [1, 1, 2, 1, 2, 3, 1, 2, 3, 4]   // pattern generator (ten rules)
[1, 2, 4].separate { | a b | (b - a) > 1 } == [[1, 2], [4]] // separate using predicate block
[0, 0, 0, 1, 1, 1].hammingDistance([0, 0, 1, 1, 0, 0]) == 3 // number places at which element differs
"xyz".editDistance("xyz") == 0                              // identity
"xyz".editDistance("xyZ") == 1                              // substitution (at end)
"xyz".editDistance("xy") == 1                               // deletion (from end)
"xy".editDistance("xyz") == 1                               // addition (at end)
"xz".editDistance("xyz") == 1                               // addition (in interior)
"xz".editDistance("xyz") == 1                               // deletion (from interior)
"xyz".editDistance("xzy") == 2                              // substitutions
"hello".similarity("hello") == 1                            // similarity based on edit distance (1 = equal)
"hello".similarity("asdf") == 0                             // similarity based on edit distance (0 = unequal)
"word".similarity("wrod") == 0.5                            // similarity based on edit distance
[[1, 2, 3], [4, 5, 6]].flop == [[1, 4], [2, 5], [3, 6]]     // matrix transposition
[[1, 4], [2, 5], [3, 6]].flop == [[1, 2, 3], [4, 5, 6]]     // matrix transposition
[[1, 2], [3, 4], [5]].flop == [[1, 3, 5], [2, 4, 5]]        // matrix transposition (shorter rows wrap)
[].flop == [[]]                                             // matrix transposition (answer is matrix)
[1, 2, 3, 4, 5, 6].shift(3, 0) == [0, 0, 0, 1, 2, 3]        // shift right with fill value
[1, 2, 3, 4, 5, 6].shift(-3, 0) == [4, 5, 6, 0, 0, 0]       // shift left with fill value
[[1, 2], [3]].allTuples == [[1, 3],[2, 3]]                  // all possible combinations of subcollections
[[1, 2], [3, 4], [5, 6]].allTuples.size == (2 * 2 * 2)
[[1, 2], [3, 4, 5], [6, 7, 8, 9]].allTuples.size == 24
[1, 2, 3].permute(4) == [2, 3, 1]                           // nth permutaton of array
[1, 2, 3].permute(5) == [3, 1, 2]
(1 .. 9).normalizeSum.sum == 1                              // array divided by it's sum
(1 .. 9).normalize(10, 90).sum == 450                       // normalize array to lie between minima and maxima
[4,7,6,8].reshape(2, 2) == [[4, 7], [6, 8]]                 // reshape array given Apl type shape value
[4,7,6,8].reshape(2, 3) == [[4, 7, 6], [8, 4, 7]]           // cycle input as required
(1 .. 5).wrapExtend(9) == ((1 .. 5) ++ (1 .. 4))            // extend array cyclically
(1 .. 5).wrapExtend(3) == (1 .. 3)                          // truncate is required
[2, 3, 5, 6].indexOfGreaterThan(4) == 2                     // index of first item greater than argument
[2, 3, 5, 6].indexInBetween(5.2) == 2.2                     // interpolated index for value (collection must be sorted)
[2, 3, 5, 6].blendAt(2.2) == 5.2                            // interpolated value between indices
[-1, 4].collect { | i | [2, 3, 5, 6].clipAt(i) } == [2, 6]  // clamp index to valid range
(1 .. 5).slide(3, 1) == [1, 2, 3, 2, 3, 4, 3, 4, 5]         // adjacent elements given windowLength and stepSize
(1 .. 6).slide(3, 2) == [1, 2, 3, 3, 4, 5]                  // 1-3, 3-5 (next would be 5-7)
(1 .. 5).slide(4, 1) == [1, 2, 3, 4, 2, 3, 4, 5]            // 1-4, 2-5 (next would be 3-6)
(1 .. 5).dupEach(2) == [1, 1, 2, 2, 3, 3, 4, 4, 5, 5]       // duplicate each element count times
[1, 2, 3, 2, 3, 4].replace([2, 3], -2) == [1, -2, -2, 4]    // replace each occurrence of a subsequence with item
[1, 2, 3, 2, 3].replace([2, 3], [7, 5]) == [1, 7, 5, 7, 5]  // splices in relacement subsequences

'Assignment'
var x; x == nil                                             // variable declaration (default value is nil)
var x, y, z; x == z                                         // variable declarations (multiple)
var x = 5; x == 5                                           // variable declaration with initializer expression
x = 5; x == 5                                               // assignment (single letter "interpreter" variables)
x = y = z = 6; x == z                                       // compound assignment
x = (y = 6) + 1; x == 7                                     // compound (expression) assignment
x = 1.2.hash; x == 1527098321                               // hash value for object (stable for numbers)
y = x.copy; x == y                                          // copy object
y = x.shallowCopy; x == y                                   // copy object (not overridden)
y = x.deepCopy; x == y                                      // copy object and instance vars
var x; x = 5; x == 5                                        // assignment (set variable x to 5)
var x, y; x = y = 5; [x, y] == [5, 5]                       // compound assignment (set variables x and y to 5)
var x, y; x = (y = 5) + 1; [x, y] == [6, 5]                 // expression assignment
var x = 1; var y = x + 1, z = y; [x, z] == [1, 2]           // multiple variable declarations

'Association'
Association.new('x', 1).class == Association                // associate key with value
'x' -> 1 == Association.new('x', 1)                         // operator syntax
('x' -> 1).key == 'x'                                       // key (left hand side) of association
('x' -> 1).value == 1                                       // value (right hand side) of association
((0 -> 1) == (0 -> 2)) == true                              // equality for associations is based on key only

'Bag'
Bag(4).class == Bag                                         // Bag, multi-set, counted-set
[1, 2, 3, 3].asBag.contents.isDictionary                    // the contents of a Bag is a Dictionary
[1, 2, 3, 3].asBag.itemCount(3) == 2                        // number of occurrences of indicated item
[1, 2, 3, 3].asBag.remove(3).itemCount(3) == 1              // remove one occurrence of indicated item from Bag
[1, 2, 3, 3].asBag.includes(3)                              // predicate to determine if a Bag contains an item
var n = 0; [1, 3, 2].asBag.do { |i| n = n.max(i) }; n == 3  // iterate over items in bag
var n = 0; "xx".asBag.countsDo { |i c| n = n.max(c) }; n == 2 // iterate over items & counts in bag

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
true.if { 'T' } == 'T'                                      // if then, c.f. conditional statements
false.if { 'T' } { 'F' } == 'F'                             // if then else (do)
true.if { 'T' } { 'F' } == 'T'                              // if then else (value)
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
#[$., $$, $ ].join == ".$ "                                 // notable characters (period, dollar, space)

'Class'
String.isKindOf(Class) == true                              // class
String.name == 'String'                                     // class name
String.class == Meta_String                                 // meta class (class of a class)
String.class.class == Class                                 // class of a meta class is Class
String.instVarNames == nil                                  // immediate instance variable names
String.classVarNames.size == 1                              // immediate class variable names
String.methods.collect { | m | m.name }.includes('++')      // message selectors for class (selectors are symbols)
String.superclass == RawArray                               // immediate superclass
String.superclasses.size == 5                               // all superclass
UGen.superclasses == [AbstractFunction, Object]             // superclass sequence
UGen.subclasses.includes(LinExp).not                        // immediate subclasses
UGen.allSubclasses.includes(LinExp)                         // accumulated subclasses
1.class == Integer                                          // class of value
1.isKindOf(Number) == true                                  // is instance of a class or a subclass
1.isMemberOf(Integer) == true                               // is instance of a class
1.respondsTo('exp') == true                                 // responds to message

'Collection'
Collection.allSubclasses.includes(Set)                      // collection class
[].isEmpty == true                                          // is collection empty
[1, 2, 3] == [1, 2, 3]                                      // are collections equal
[9, 4, 5, 7, 8, 6].size == 6                                // size of collection
[3.141, 23, 'symbol'].collect { | k | k.size } == [0, 0, 0] // size non collections answers zero
[9, 4, 5, 7, 8, 6].maxItem == 9                             // maximum item in collection
[9, 4, 5, 7, 8, 6].minItem == 4                             // minimum item in collection
[9, 4, 5, 7, 8, 6].sum == 39                                // sum of collection (inject 0 +)
[9, 4, 5, 7, 8, 6].mean == 6.5                              // sum of collection divided by size
[9, 4, 5, 7, 8, 6].product == 60480                         // product of collection (inject 1 *)
[9, 4, 5, 7, 8, 6].inject(0) { | z e | e + z } == 39        // sum of collection
[9, 4, 5, 7, 8, 6].inject(1) { | z e | e * z } == 60480     // product of collection
[9, 4, 5, 7, 8, 6].includes(7) == true                      // is element in collection
[9, 4, 5, 7, 8, 6].count { | item | item.even } == 3        // count elements that satisfy predicate
[9, 4, 5, 7, 8, 6].any { | item | item.even } == true       // do any elements satisfy predicate
[9, 4, 5, 7, 8, 6].every { | item | item.even } == false    // do all elements satisfy predicate
[9, 4, 5, 7, 8, 6].occurrencesOf(7) == 1                    // count elements that are equal to object
[1, 2, 3].union([3, 4, 5]) == [1, 2, 3, 4, 5]               // combine two collections
[1, 2, 3].isSubsetOf([1, 2, 3, 4, 5]) == true               // is one collection a subset of the other
[1, [2, 3], [4, [5, 6]]].maxDepth == 3                      // tree depth of collection
[1, [2], [3, [4]]].deepCollect(3) { | x | x.neg }.size == 3 // tree collect to indicated depth
[1, 2, 3, 4, 5].choose <= 5                                 // random element of collection
[1, 2, 3, 4, 5].wchoose([1, 2, 3, 4, 5] / 15) <= 5          // weighted random element of collection (weights sum to 1)
[1, 2].powerset == [[], [1], [2], [1, 2]]                   // set of all subsets
(1 .. 4).powerset.size == 16                                // set of all subsets (size is 2 ** n)
a = (1 .. 9); a.reject { | each | each.even } == (1, 3 .. 9) // reject elements
a.removeAllSuchThat { | each | each.even }; a == (1, 3 .. 9) // reject in place

'Comments'
"until end of line comment".isString                        // until end of line comment
"multi-line or interior-line comment".isString              // /* multi-line or interior-line comment */

'Complex'
Complex(0, 0).class == Complex                              // complex numbers
Complex(-1, 0).real == -1                                   // real part of complex number, Complex(r, i) = Complex.new(r, i)
Complex(-1, 0).imag == 0                                    // imaginary part of complex number
Complex(-1, 0) + 1 == Complex(0, 0)                         // complex addition with scalar
var c = Complex(0, 1); c * c == Complex(-1, 0)              // complex multiplication
Complex(-1, 0).pow(1 / 2).real < 0.00001                    // complex raisedTo

'Conditional Statements'
var x = (9.rand > 4).if { 'T' }; x.isNil || { x == 'T' }    // if then
var x = (9.rand > 4).if { $T } { $F }; "TF".includes(x)     // if then else
var x = (9.rand > 4).if($T, $F); "TF".includes(x)           // if then else
var x = 2.rand.switch(0, { $A }, 1, $B); "AB".includes(x)   // switch

'Constants'
true                                                        // true constant
false.not                                                   // false constant
nil.isNil                                                   // nil object constant
inf.class == Float                                          // Infinity constant (instance of Float)
pi.class == Float                                           // pi constant (instance of Float)
1.isNumber                                                  // integer constants
3.14.isNumber                                               // float constants
-1.isNumber                                                 // negative integer constants
-3.14.isNumber                                              // negative float constants
"Hello".isString                                            // string constant (\ quotes quote)
$a.class == Char                                            // character constant
$ .isSpace                                                  // character constant (space)
'aSymbol'.class == Symbol                                   // symbol constants
'a Symbol'.class == Symbol                                  // symbol constants (space)
\aSymbol == 'aSymbol'                                       // prefix symbol notation
#[3, 2, 1].size == 3                                        // array constants
#['a', "a", $a, 1].size == 4                                // mixing of types allowed

'Conversion'
1.0.asInteger == 1                                          // convert number to integer (truncates in Squeak)
1.5.asFraction == [3, 2]                                    // convert number to fraction
pi.asFraction(10, false) == [22, 7]                         // convert number to fraction (with maximum denominator)
3.asFloat == 3.0                                            // convert number to float
97.asAscii == $a                                            // convert integer to character
$a.ascii == 97                                              // convert character to integer
$a.asString == "a"                                          // convert object to string
$a.asCompileString == "$a"                                  // convert object to string via storeOn:
15.asHexString(2) == "0F"                                   // convert to string in hex base

'Date'
Date.getDate.class == Date                                  // current date and time
Date.localtime.class == Date                                // current local date and time
Date.localtime.rawSeconds.isFloat == true                   // fractional seconds
Date.localtime.stamp.isString == true                       // timestamp as string
Date.new(2020, 04, 12, 14, 57, 10, 1, nil).asString; true   // Mon Apr 12 14:57:10 2020

'Dictionary'
Dictionary.superclass == Set                                // dictionaries are sets
d = Dictionary.new(4); d.isEmpty                            // new empty dictionary
d.add('x' -> 1).add('y' -> 2).add('z' -> 3).size == 3       // add association to dictionary
d == Dictionary.newFrom(['x', 1, 'y', 2, 'z', 3])           // new Dictionary from sequence of adjacent key value pairs
d == Dictionary.with('x' -> 1, 'y' -> 2, 'z' -> 3)          // new Dictionary from associations ; c.f. Dictionary.with(*['x' -> 1, 'y' -> 2, 'z' -> 3])
d.asArray == [2, 1, 3]                                      // values (not keys or associations) as array
n = 0; d.associationsDo { | x | n = n + x.value }; n == 6   // run f at associations of dictionary
d.at('x') == 1                                              // lookup value at key
d.at('_') == nil                                            // lookup value at key, nil if key not present
var n = 0; d.do { | x | n = n + x }; n == 6                 // run f at values of dictionary
d.includes(3) == true                                       // does dictionary have indicated value
d.includesKey('z') == true                                  // does dictionary have indicated key
d.keys == Set.newFrom(['x', 'y', 'z'])                      // set of keys at dictionary
var n = 0; d.keysDo { | x | n = n + d[x] }; n == 6          // run f at keys of dictionary
d.keysValuesDo { | k v | (d[k] != v).if { 'x'.error } } == d// run f at keys & values of dictionary
d.occurrencesOf(3) == 1                                     // number of occurrences of value in dictionary
{ d.removeAt('x') == 1; d.add('x' -> 1) }.value.size == 3   // remove key (and return value of) from dictionary
d.size == 3                                                 // size of dictionary (number of associations)
d.values.sort { | i j | i <= j }.asArray == (1 .. 3)        // list of values at dictionary
var d = (c: 1, b: 2, a: 3); d.order == ['a', 'b', 'c']      // sorted array of keys
var d = (c: 1, b: 2, a: 3); d.atAll(d.order) == [3, 2, 1]

'Environment'
Environment.superclass == IdentityDictionary                // environments are dictionaries
Object.classVarNames.includes('currentEnvironment') == true // currentEnvironment is a class variable of Object
'key'.envirPut('value') == 'value'                          // put 'key' -> 'value' into currentEnvironment
'key'.envirGet == 'value'                                   // lookup 'key' in currentEnvironment
f = { 'i'.envirGet + 'j'.envirGet }; f.isFunction           // function to lookup and sum i and j
g = { | result | result.errorString }; g.isFunction         // function to retrieve error message
{ f.value }.try { | e | e.class == DoesNotUnderstandError } // are i and j in currentEnvironment and non nil?
e = Environment.newFrom(['i', 4, 'j', 5] ); true            // new environment with 'i' -> 4 and 'j'-> 5
e.use(f) == 9                                               // evaluate f with currentEnvironment of e => 4 + 5 = 9
c = Environment.new; true                                   // new empty environment
{ c.use(f) }.try { true }                                   // i and j aren't in c, => nil + nil = error
c.parent_(e); true                                          // set e as parent of c, c is child of e
c.use(f) == 9                                               // c is empty, look through c into e => 4 + 5 = 9
c.put('j', 7) == c                                          // set j in c
c.use(f) == 11                                              // i from e and j from c => 4 + 7 = 11
h = { ('i'.envirGet ? 3) * ('j'.envirGet ? 3) }; true       // function to lookup and multiply i and j with defaults
e.use(h) == 20                                              // => 4 * 5 = 20
c.use(h) == 28                                              // => 4 * 7 = 28
h.value == 9                                                // => 3 * 3 = 9

'Floating point'
1.0 * 3.0 > 3.0 == false                                    // integral floating point math
0.1 * 3.0 > 0.3 == true                                     // general floating point math
1.0 * 3.0 == 3.0 == true                                    // integral floating point math
0.1 * 3.0 == 0.3 == false                                   // general floating point math
1.0 + 1.0 + 1.0 == 3.0 == true                              // integral floating point math
0.1 + 0.1 + 0.1 == 0.3 == false                             // general floating point math
5.linlin(0, 10, -4.3, 100).round == 48                      // linear to linear mapping
5.linexp(0, 10, 4.3, 100).round == 21                       // linear to exponential mapping
5.explin(0.1, 10, -4.3, 100).round == 84                    // exponential to linear mapping
5.expexp(0.1, 10, 4.3, 100).round == 62                     // exponential to exponential mapping
5.lincurve(0, 10, -4.3, 100, -3).round == 81                // linear to parametric curve mapping
5.curvelin(0, 10, -4.3, 100, -3).round == 18                // parametric curve to liner mapping

'IdentityDictionary'
IdentityDictionary.superclass == Dictionary                 // identity dictionaries are dictionaries
d = IdentityDictionary.newFrom(['i', 1, 'j', 2]); true      // new dictionary from array
d.asArray == [2, 1]                                         // values as array
d.size == 2                                                 // number of entries
c = IdentityDictionary.new; true                            // new empty dictionary
c.proto_(d) == c                                            // there is a proto variable
c.at('i') == 1                                              // lookup searches proto
c.size == 0                                                 // size does not include proto
e = IdentityDictionary.newFrom(['j', -2, 'k', -3]); true    // new dictionary (from array)
c.parent_(e) == c                                           // there is a parent variable, set to e
c.at('j') == 2                                              // lookup is in proto before parent
c.at('k') == -3                                             // lookup is in parent after proto
c.put('i', 'I') == c                                        // at & put protocol
c.at('i') == 'I'                                            // lookup is at self before proto
c.keys == Set.newFrom(['i'])                                // only "local" elements are considered
c.collect { | item | nil }.at('j') == 2                     // only "local" elements, parent and proto are copied

'Integer'
127.asBinaryDigits(8) == [0, 1, 1, 1, 1, 1, 1, 1]           // integer as big endian binary digits to n places
(0 .. 255).collect { | n | n.asBinaryDigits(8) }.size == 256// all eight bit integers
23.asDigits(2, 5) == [1, 0, 1, 1, 1]                        // binary digits
23.asDigits(8, 2) == [2, 7]                                 // octal digits
23.asDigits(10, 2) == [2, 3]                                // decimal digits
23.asDigits(16, 2) == [1, 7]                                // hexadecimal digits
[1, 0, 1, 1, 1].convertDigits(2) == 23
[2, 7].convertDigits(8) == 23
[2, 3].convertDigits(10) == 23
[1, 7].convertDigits(16) == 23

'Interpreter'
this.class == Interpreter                                   // context in which expressions are evaluated
this.interpret("1 + 2") == 3                                // interpret string
this.interpret("{ | x | x + 2 }").value(1) == 3             // interpret string
this.r == nil                                               // there are single letter interpreter variables
this.s == Server.default                                    // by convention s has the default scsynth
this.executeFile("program.scd"); true                       // run program tex
"['c' -> 60, 'b' -> 50, 'a' -> 100]".interpret.isArray      // interpret (read) data structure
context.class == Frame                                      // context

'Interval'
Interval.new(0, 12, 3).asArray == [0, 3, 6, 9, 12]          // elements of interval as array
Interval.new(0, 12, 3).size == 5                            // number of elements in interval
2.to(14, 4).collect { | x | x * x } == [4, 36, 100, 196]    // to method at Integer
Interval.new(-1, 1, 0.000001).size == 2000001               // 2000001 places between -1 and 1
Interval.new(-1, 1, 0.000001).at(1000000) == 0              // access value at 1000001st place

'Iteration'
var n = 0; 4.do { n = n + 1 }; n == 4                       // times repeat loop (int)
var n = 0; 4.do { | x | n = n + x }; n == 6                 // times repeat loop (int)
var n = 0; 4.do { | x | n = n + x }; n == 6                 // for loop (int)
var s = ""; 4.do { | x | s = s ++ x }; s == "0123"          // for loop (int)
var s = ""; 4.reverseDo { | x | s = s ++ x }; s == "3210"   // for loop (int, reversed)
var s = ""; 1.to(5).do { | x | s = s ++ x }; s == "12345"   // for loop (interval)
var s = ""; [1, 3, 5].do { | x | s = s ++ x }; s == "135"   // for loop (collection)
var s = ""; 1.for(5) { | x | s = s ++ x }; s == "12345"     // for loop
var s = ""; 1.forBy(9, 2) { | x | s = s ++ x }; s == "13579"// for by loop
var n = 9; { n > 3 }.while { n = n - 1 }; n == 3            // while loop

'Files'
File.open("/tmp/x", "w").put('x').close.class == File       // open file, put value, close file
File.use("/tmp/x", "w") { | f | f.put('x') }.class == File  // use file to put value
File.exists("/tmp/x") == true                               // file exists predicate
File.fileSize("/tmp/x") == 1                                // file size (bytes)
File.readAllString("/tmp/x") == "x"                         // read file contents (string)
File.use("/tmp/x", "r") { | f | f.getChar } == $x           // use file to read character
File.delete("/tmp/x")                                       // delete file

'Functions (Blocks)'
var x = { }; x.isFunction                                   // blocks are objects and may be assigned to a variable
{ nil; 1 }.value == 1                                       // value is last expression evaluated unless ^ (return)
{ }.value == nil                                            // empty blocks answer nil
{ var x; }.value == nil                                     // also with unused temporaries
{ var x = 1; }.value == nil                                 // also with unused initialised temporaries
{ 'x' }.value == 'x'                                        // apply no argument block
{ | n | n * n }.value(23) == 529                            // apply one argument block
{ | p q | p ++ " & " ++ q }.value("x", "y") == "x & y"      // apply two argument block
{ | n | n * n }.value(23, 3.141) == 529                     // apply one argument block to two arguments
{ { 1 }.value }.value == 1                                  // blocks may be nested
{ | x | var y = x; y }.value(1) == 1                        // specification { arguments localvars expressions }
{ | x | x + 1 }.def.sourceCode == "{ | x | x + 1 }"         // the FunctionDef can be retrieved
({ | x | x * 2 } <> { | x | x + 3 }).value(4) == 14         // function composition, right to left
{ | x | x = nil; x }.value(42).isNil                        // arguments are mutable
{ |f| 10.do { |i| (i == 5).if { f.value(5) } } }.block == 5 // non-local return

'J Concepts'
(z = Array.iota(3, 2) + 1) == [ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ] ]
z.bubble(0, 1) == [z]
z.bubble(1, 1) == [[[1, 2]], [[3, 4]], [[5, 6]]]
z.bubble(2, 1) == [[[1], [2]], [[3], [4]], [[5], [6]]]
z.bubble(0, 0) == z.bubble(0, 1)
z.bubble(0, 1).unbubble(0, 1) == z
z.bubble(1, 1).unbubble(1, 1) == z
z.bubble(2, 1).unbubble(2, 1) == z
z.bubble(0, 2) == [[z]]
z.bubble(0, 2).unbubble(0, 2) == z
z.bubble(1, 2).unbubble(1, 2) != z
z.bubble(2, 2).unbubble(2, 2) != z
[[1, 2, 3], [[41, 52], 5, 6], 1, 2, 3].maxDepth == 3
(0 .. 3).collect { | k | [[1, 2, 3], [[41, 52], 5, 6], 1, 2, 3].maxSizeAtDepth(k) } == [5, 3, 2, 1]
(z = Array.iota(4, 5) + 1).size == 4
z.slice(nil, (1 .. 3)) == [[2, 3, 4], [7, 8, 9], [12, 13, 14], [17, 18, 19]]
z.slice(2, (1 .. 3)) == [12, 13, 14]
z.slice((2 .. 3), (0 .. 2)) == [[11, 12, 13], [16, 17, 18]]
z.slice((1 .. 3), 3) == [9, 14, 19]
z.slice(2, 3) == 14
(z = Array.iota(3, 3, 3) + 1).size == 3
z.slice([0,1], [1,2], [0,2]) == [[[4, 6], [7, 9]], [[13, 15], [16, 18]]]
z.slice(nil, nil, [0,2]) == [[[1, 3], [4, 6], [7, 9]], [[10, 12], [13, 15], [16, 18]], [[19, 21], [22, 24], [25, 27]]]
z.slice(1) == [[10, 11, 12], [13, 14, 15], [16, 17, 18]]
z.slice(nil, 1) == [[4, 5, 6], [13, 14, 15], [22, 23, 24]]
z.slice(nil, nil, 0) == [[1, 4, 7], [10, 13, 16], [19, 22, 25]]
z.slice(nil, 2, 1) == [8, 17, 26]
z.slice(nil, 1, (1 .. 2)) == [[5, 6], [14, 15], [23, 24]]
z.slice(1, [0,1]) == [[10, 11, 12], [13, 14, 15]]
(z = Array.iota(2, 3, 2) + 1).shape == [2, 3, 2]
(f = { | item | item.reverse }).isFunction
z.collect(f) == [[[5, 6], [3, 4], [1, 2]], [[11, 12], [9, 10], [7, 8]]]
z.deepCollect(0, f) == [[[7, 8], [9, 10], [11, 12]], [[1, 2], [3, 4], [5, 6]]]
z.deepCollect(1, f) == z.collect(f)
z.deepCollect(2, f) == [[[2, 1], [4, 3], [6, 5]], [[8, 7], [10, 9], [12, 11]]]
(a = [[10, 20],[30, 40, 50], 60, 70, [80, 90]]).shape == [5, 2]
(b = [[1, 2, [3, 4], [[5], 6], 7], 8, [[9]]]).shape == [3, 5]
a.reshapeLike(b) == [[10, 20, [30, 40], [[50], 60], 70], 80, [[90]]]
b.reshapeLike(a) == [[1, 2], [3, 4, 5], 6, 7, [8, 9]]

'List'
List.new.isKindOf(Collection) == true                       // c.f. Collection
List.new.isKindOf(SequenceableCollection) == true           // c.f. SequenceableCollection
List.newUsing([1, 3, 2, 4, 5]).size == 5                    // list from array
[1, 3, 2, 4, 5].asList.size == 5                            // list from collection
[1, 3, 2, 4, 5].asList.asArray == [1, 3, 2, 4, 5]           // list to array
var l = [1, 3, 2, 4, 5].asList; l.reverse.reverse == l      // list reverse (copying)
(1 .. 5).asList.add(6).asArray == (1 .. 6)                  // add element at end (snoc)
(2 .. 6).asList.addFirst(1).asArray == (1 .. 6)             // add element at start (cons)
var l = [1, 3, 2, 4].asList; (l.pop == 4) && (l.size == 3)  // remove (in place) and return last element
var l = [1, 2, 4].asList.put(2, 3); l[2] == 3               // replace (in place) element at index
var l = [1, 2, 4].asList.insert(2, 3); l.size == 4          // insert (in place) element at index
var l = [1, 2, 4].asList; [l.removeAt(2), l.size] == [4, 2] // remove (in place) and return element at index
var l = [1, 2, 3].asList; l.clear; l.isEmpty                // remove all items from collection
List.new.add(1) == [1].asList                               // add element to list, return list
List.new(4).size == 0                                       // new with size makes empty list
List.newClear(4).size == 4                                  // newClear makes list of nil values
List.new.species == Array                                   // Lists species is Array

'Magnitude'
1 < 3 == true                                               // less than
3 <= 3 == true                                              // less than or equal to
3 > 1 == true                                               // greater than
1 >= 1 == true                                              // greater than or equal to
2 == 2 == true                                              // equal to

'Math'
-3.abs == 3                                                 // absolute value
1.5.ceil == 2                                               // ceiling (round up)
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
pi - 3.1415926535898 < 0.00001                              // constant pi (Float pi)
inf.isNumber                                                // constant positive infinity (is a number)
2.pow(3) == 8                                               // i to the power of j
5.reciprocal == 0.2                                         // 1 / x
(pi/2).sin == 1                                             // sine
9.sqrt == 3                                                 // square root
3.squared == 9                                              // x * x
pi.raddeg == 180                                            // radiansToDegrees

'Message sends'
9.sqrt == 3                                                 // unary message
1.min(2) == 1                                               // n-ary message (keyword message in St)
1 + 2 == 3                                                  // binary operator method
9.perform('sqrt') == 3                                      // indirect message send (unary)
1.perform('min', 2) == 1                                    // indirect message send (binary)
1.perform('+', 2) == 3                                      // indirect message send (operator)
1.exp.log == 1                                              // unary method chaining (log of exp of 1)
6561.sqrt.sqrt.sqrt == 3                                    // unary method chaining
1.min(2).max(3) == 3                                        // n-ary method chaining

'MultiLevelIdentityDictionary'
m = MultiLevelIdentityDictionary.new; m.isEmpty             // Tree structure, dictionary of dictionaries
m.put('a', 'b', 'c', 1) == m                                // Put value at path (path as variable arguments)
m.putAtPath(['a', 'b', 'd'], 2) == m                        // Put value at path (path as array)
m.at('a', 'b', 'd') == (m.atPath(['a', 'b', 'c']) + 1)      // Get value at path
m.atPathFail(['a', 'b', 'e']) { "absent" } == "absent"      // Get value at path with ifAbsent handler
m.at('a', 'b').keys == ['c', 'd'].asSet                     // Get sub-dictionary at path
m.leaves == [[[2, 1]]]                                      // Get all leaves as nested array
m.leaves.flat.sort == [1, 2]                                // Get all leaves in flat sorted array
m.leafDo { | path | (path -> m.atPath(path)).post } == m    // Apply function at path to every leaf node
m.treeDo { | path | path.post } == []                       // Apply function at path to every non-leaf node
m.postTree == m                                             // Print pretty tree
m.removeAt('a', 'b', 'c') == 1                              // Remove value at path
m.at('a', 'b', 'c') == nil                                  // Nil indicates path exists
m.removeEmptyAt('a', 'b', 'd') == 2                         // Remove value at path, and all empty nodes along path
m.dictionary.size == 0                                      // Get root dictionary

'Nil'
nil.class == Nil                                            // nil
nil ? 'x' == 'x'                                            // right hand side if nil
'x' ? 'y' == 'x'                                            // left hand side unless nil
nil.isNil == true                                           // is nil

'Numbers'
23.isInteger                                                // decimal integer literal
2r10111 == 23                                               // binary (base 2) integer literal
8r27 == 23                                                  // octal (base 8) integer literal
10r23 == 23                                                 // decimal (base 10) integer literal
16r17 == 23                                                 // hexadecimal (base 16) integer literal
-23.isInteger                                               // negative decimal integer literal
-2r10111 == -23                                             // negative binary (base 2) integer literal
-8r27 == -23                                                // negative octal (base 8) integer literal
-10r23 == -23                                               // negative decimal (base 10) integer literal
-16r17 == -23                                               // negative hexadecimal (base 16) integer literal

'Point'
200 @ 100 == Point.new(200, 100)                            // obtain a new point
(200 @ 100).x == 200                                        // x coordinate
(200 @ 100).y == 100                                        // y coordinate
0 - (200 @ 100) == (-200 @ -100)                            // negates x and y (note @- is an binary selector)
(0 - (200 @ 100)).abs == (200 @ 100)                        // absolute value of x and y
200 @ 100 + 1 == (201 @ 101)                                // add scale to both x and y
200 @ 100 - 1 == (199 @ 99)                                 // subtract scale from both x and y
200 @ 100 * 2 == (400 @ 200)                                // multiply x and y by scale
200 @ 100 / 2 == (100 @ 50)                                 // divide x and y by scale
(200 @ 100).div(2) == (100 @ 50)                            // divide x and y by scale
200 @ 100 % 3 == (2 @ 1)                                    // modulo of x and y by scale
200 @ 100 + (50 @ 25) == (250 @ 125)                        // add points
200 @ 100 - (50 @ 25) == (150 @ 75)                         // subtract points
200 @ 100 * (3 @ 4) == (600 @ 400)                          // multiply points
1800 @ 100 / (3 @ 4) == (600 @ 25)                          // divide points
(200 @ 100).asArray == [200, 100]                           // array of x and y

'Post'
"text".postln == "text"                                     // write text to post window (console)
'sym'.postln == 'sym'                                       // write object to post window (console)

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
{ 9.rand }.dup(9).size == 9                                 // random integers (0 to self)
{ 0.9.rand }.dup(23).maxItem < 0.9                          // random floats (0 to self)
#[1, 2, 3, 4, 5].choose.isInteger                           // random element of collection
#[1, 2, 3, 4, 5].wchoose(#[1, 2, 3, 4, 5] / 15).isInteger   // weighted random element of collection (weights sum to 1)
{ 3.rrand(9) }.dup(9).minItem >= 3                          // random integer in range
{ 0.3.rrand(0.9) }.dup(23).maxItem < 0.9                    // random float in range

'Routine'
f = { | x | var y = x * 2, z = y.yield; f.value(z) }; true  // yield the receiver to the coroutine (received by next)
var r = Routine(f); r.class == Routine                      // a routine is instantiated on a function
(1 .. 9).collect { | i | i -> r.next(i) }.at(6) == (7 -> 14)// sends value to coroutine (also resume, value, run)

'SequenceableCollection'
SequenceableCollection.allSubclasses.includes(List)         // sequencable collection class
[1, 3, 2] ++ [4, 5] == [1, 3, 2, 4, 5]                      // append sequences
[1, 3, 2, 4, 5].reverse == [5, 4, 2, 3, 1]                  // reverse sequence
[1, 3, 2, 4, 5].sort == [1, 2, 3, 4, 5]                     // sort using default comparison
[1, 3, 2, 4, 5].sort { | i j | i > j }.first == 5           // sort using provided comparison
[3, 3, 3, 2, 2, 1].sort.size == 6                           // sort retains duplicates
var c = [3, 2, 1]; c.sort ; c == [1, 2, 3]                  // sort is in place (mutating)
[1, 2, 3, 4, 5].keep(3) == [1, 2, 3]                        // keep first n items
[1, 2, 3, 4, 5].drop(2) == [3, 4, 5]                        // discard first n items
[1, 3, 5, 7, 9].isSeries == true                            // is array an arithmetic series
[1, 3, 5, 7, 9].isSeries(2) == true                         // is array an arithmetic series (by)
[1, 3, 5, 7, 9].resamp0(9) == [1, 3, 3, 5, 5, 7, 7, 9, 9]   // resampler
[1, 3, 5, 7, 9].resamp1(9) == [1, 2, 3, 4, 5, 6, 7, 8, 9]   // linear interpolating resampler
[1, 3, 5, 7, 9].copyRange(2, 4) == [5, 7, 9]                // copy part of collection
[1, 3, 5, 7, 9].find([5, 7, 9]) == 2                        // locate index of subsequence
[1, 3, 5, 7, 9].indexOf(5) == 2                             // index of element (compared using ==)
[1, 3, 5, 7, 9].first == 1                                  // first element of
[].first == nil                                             // first element of empty array is nil
var a = (1 .. 9); a.first == a[0]                           // zero-indexed
[1, 3, 5, 7, 9].last == 9                                   // last element of
var a = (1 .. 9); a.last == a[8]                            // zero-indexed
[1, 3, 5, 7, 9].middle == 5                                 // middle element of
[1, 3, 5, 7, 9].clump(3) == [[1, 3, 5], [7, 9]]             // groups of n elements
[1, 3, 5, 7, 9].clumps([2, 1]) == [[1, 3], [5], [7, 9]]     // groups of n elements
[1, 3, 5, 7, 9].clump(2).flatten == [1, 3, 5, 7, 9]         // concatentate
[1, 3, 5, 7, 9].integrate == [1, 4, 9, 16, 25]              // incremental sum
[1, 4, 9, 16, 25].differentiate == [1, 3, 5, 7, 9]          // pairwise difference
[0, 2, 4, 5, 7, 9, 11].mode(5, 12) == [0, 2, 3, 5, 7, 8, 10]// rotate and transpose scale
List.rand(4, 1, 9).minItem >= 1                             // list of random numbers (uniform distribution)
List.linrand(4, 1, 9).first.isInteger                       // list of random numbers (linear distribution)
List.exprand(4, 1, 9).first.isInteger.not                   // list of random numbers (exponential distribution)
[1, 2, 3].wrapAt(3) == 1                                    // index with wrap-around
[1, 2, 3].foldAt(3) == 2                                    // index with fold-around
List.rand(4, 1, 9).asArray.class == Array                   // as array
[3, [1, 2, 1], 1].convertRhythm == List[0.75, 1.5, 0.75]    // variant of Mikael Laurson's notation
[10, 20].obtain(2, 30) == 30                                // at, or default value for out of range index
23.obtain(0, nil) == 23                                     // obtain is defined at Object
[10, 20].instill(2, -30, nil) == [10, 20, -30]              // atPut, or extends array if required
23.instill(1, 3.141, nil) == [23, 3.141]                    // instill is defined at Object
[1, 3, 2, 5, 4].order == [0, 2, 1, 4, 3]                    // indices that would sort recievr

'Set'
Set.new.isKindOf(Collection) == true                        // c.f. Collection
Set.new(4).size == 0                                        // allocate empty set for up to 4 elements
[1, 2, 3, 4].asSet.size == 4                                // set from array
Set.new(4).add(1).add(2).add(3) == [1, 2, 3].asSet          // add elements to collection
Set.new(3).addAll([1, 2, 3]) == [1, 2, 3].asSet             // add all elements of collection to collection
[1, 2, 3, 4].asSet.remove(3) == [1, 2, 4].asSet             // remove element from collection
[1, 2, 3, 4].asSet.removeAll([3, 4]) == [1, 2].asSet        // remove all elements of collection from collection
[1, 2, 3, 4].asSet.includes(3) == true                      // test if element is in collection
[1, 2, 3, 4].asSet.size == 4                                // number of elements
[1, 2, 3].asSet.sect([2, 3, 4].asSet) == [2, 3].asSet       // set intersection (p + q)
[1, 2].asSet.union([2, 3].asSet) == [1, 2, 3].asSet         // set union (p & q)
[1, 2, 3].asSet.difference([2, 3, 4].asSet) == [1].asSet    // set difference (p - q)
[1].asSet.symmetricDifference([2].asSet) == [1, 2].asSet    // symmetric difference ((p - q) + (q - p))
[2, 3].asSet.isSubsetOf([1, 2, 3, 4].asSet) == true         // subset of
[1, 1, 2, 2, 3, 3].asSet.asArray.size == 3                  // remove duplicates from array
[1, 1, 2, 2, 3, 3].asSet.asArray.sort == [1, 2, 3]          // remove duplicates from array and sort

'SortedList'
[2, 1, 5, 3].asSortedList.asArray == [1, 2, 3, 5]           // sorted list from array, to array
[1, 2, 5, 3].asSortedList.add(4).asArray == (1 .. 5)        // add element to a sorted list
[1, 2, 5, 3].asSortedList.addAll([4, 6]).asArray == (1 .. 6)// add all elements of a collection to a sorted list

'String'
"quoted string".isString                                    // quoted string
"x" ++ "y" == "xy"                                          // catenation
"x" + "y" == "x y"                                          // catenation with space
"/usr" +/+ "local" == "/usr/local"                          // file path catenation
"string".ascii == [115, 116, 114, 105, 110, 103]            // String -> [Int]
"3.4".asFloat == 3.4                                        // parse float
"3".asInteger == 3                                          // parse integer
"string".asSymbol == 'string'                               // String -> Symbol
"string".at(4) == $n                                        // indexing
"string" @ 4 == $n                                          // indexing (@ notation)
"".isEmpty == true                                          // empty string predicate
"string".isEmpty == false                                   // is empty string
"string".mutable == false                                   // string literals are immutable
'string'.asString.put(3, $u) == "strung"                    // mutate string setting char at indexed slot
'string'.asString.removeAt(5) == $g                         // mutate string removing char at slot
"string".size == 6                                          // length
["m", "ss", "ss", "pp", ""].join($i) == "mississippi"       // join using character
["m", "ss", "ss", "pp", ""].join("i") == "mississippi"      // join using string
"mississippi".split($i) == ["m", "ss", "ss", "pp", ""]      // split at char (not string)
"str ing".split($ ) == ["str", "ing"]                       // split at char
"a b=2".split($ ).collect { | e | e.split($=) }[1][1] == "2"// split as parser
"a" < "b" == true                                           // string comparison
"dcba".copy.sort == "abcd"                                  // inplace string sort
"text".copyRange(1,2) == "ex"                               // substring
"text".copyRange(2,2) == "x"                                // substring (single character)
"% string % interpolation".format('format', 'with').isString// format string with interpolation
{ "string".add($!) }.try { "oh oh..." } == "oh oh..."       // literal strings are immutable arrayed collections
["", "string"].collect('maxSize') == [16, 64]               // of minimum maxSize 16
var s = "string".copy; s.add($!); s.at(6) == $!             // copied string literals inherit maxSize
"String".asArray.class != Array                             // https://github.com/supercollider/supercollider/issues/5658

'Symbol'
'symbol'.class == Symbol                                    // symbol
Symbol.superclass == Object                                 // Symbol is not a subclass of String
'symbol'.asString == "symbol"                               // convert to string
'x y'.asCompileString == "'x y'"                            // convert to quoted string
'a' ++ 'b' == "ab"                                          // Symbol -> Symbol -> String, c.f. (#a , #b) = 'ab'
'a' < 'b' == true                                           // symbol ordering
'Symbol'.asArray.class == Array

'Temporaries'
var x; x == nil                                             // uninitialised variables are nil
var x = 1, y = 2; [x, y] == [1, 2]                          // var can introduce multiple temporaries
var x; var y = 0, z; [x, y, z] == [nil, 0, nil]             // there can be multiple var sequences

'Collection access and mutation syntax'
"text".copy[2].toUpper == $X                                // c[k] is syntax for c.at(k), lookup char in string
x = "text".copy; x[2] = $s; x == "test"                     // c[k] = v is syntax for c.put(k, v), set char in mutable string
x = #[1, 2, 3].copy; x[1] = $2; x == [1, $2, 3]             // set slot in mutable array

'Event (Dictionary) syntax'
(a: 1, b: 2) == Event.newFrom(['a', 1, 'b', 2])             // (a: 1, ...) is syntax for Event.newFrom(['a', 1, ...])
() == Event.new                                             // empty dictionary

'Tilde Syntax'
'i'.envirPut('j'.envirPut(3)) == 3                          // ~i = ... is syntax for 'i'.envirPut(...)
'i'.envirGet * 'j'.envirGet == 9                            // ~i is syntax for 'i'.envirGet

'Setter Syntax'
Point.new.x_(9).x == 9                                      // p.x = y is syntax for p.x_(y)

'Debugging'
{ nil.doesNotUnderstand('unknownMessage') }.try { true }    // flag message is not handled
{ nil.shouldNotImplement }.try { true }                     // flag message should not be implemented
{ nil.subclassResponsibility }.try { true }                 // flag message as abstract
{ nil.primitiveFailed }.try { true }                        // system primitive failed

'Debugging - Printing - !'
"Text".error == "Text"                                      // print error string
"Text".reportError == "Text"                                // print error string and print backtrace
Magnitude.dumpClassSubtree == Magnitude                     // print class tree below
Collection.dumpByteCodes('select') == Collection            // print bytecodes for message
UGen.dumpSubclassList == UGen                               // print sub classes (as list)
Number.dumpFullInterface == Number                          // print methods per class
SinOsc.dumpMethodList == SinOsc                             // print methods alphabetically
Class.allClasses.printAll.size > 1000                       // print all class names

'Debugging - Qt - !'
Platform.hasQt.if { String.browse }; true                   // browse specified class (Ui)
Platform.hasQt.if { Server.default.inspect }; true          // open object inspector window (Ui)
Platform.hasQt.if { String.methods.inspect }; true          // methods for class (Ui)

'Plotting - Qt - !'
Platform.hasQt.if { Env.sine.plot(size: 400) }; true        // Env ; plot
Platform.hasQt.if { Env.sine.asSignal(400).plot }; true     // Env as discrete array ; plot
1.to(9).asArray.mirror.plot                                 // Array ; plot
[1, 3, 2, 6, 5, 4, 7].pyramid(9).mirror.plot                // Array ; plot
1.to(99).collect { arg n; n.asBinaryDigits(8) }.flat.plot   // Array ; plot
{ SinOsc.ar(100, 0) }.plot(1 / 100)                         // Synthesis function ; plot
{ | x | x.sin }.plotGraph(n: 400, from: 0, to: 2 * pi)      // Unary function ; plot
{ 99.0.rand }.dup(9999).histo.plot                          // uniform distribution between zero and n
{ 99.0.linrand }.dup(9999).histo.plot                       // linear distribution between zero and n biased to zero
{ 99.0.bilinrand }.dup(9999).histo.plot                     // bilinear distribution between -n and n, biased to zero

'Method Return - !'
{ ^1 }.value                                                // ^ terminates block & method (exits all nested blocks)

'Compiler - !'
thisProcess.recompile                                       // recompile the class library
