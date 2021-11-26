'A Terse Guide to Smalltalk - c.f. http://wiki.squeak.org/squeak/5699 by Chris Rathman'

'Arithmetic expressions'

6 + 3 = 9                                                   "addition"
6 - 3 = 3                                                   "subtraction"
6 * 3 = 18                                                  "multiplication"
1 + 2 * 3 = 9                                               "evaluation always left to right (1 + 2) * 3"
(5 / 3) class = Fraction                                    "division with fractional result"
5.0 / 2.0 = 2.5                                             "division with float result (operand float)"
355.0 / 113                                                 "division with float result (operand integer)"
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
180 degreesToRadians = Float pi                             "convert degrees to radians"
Float pi radiansToDegrees = 180.0                           "convert radians to degrees"
(Float pi / 2) sin = 1.0                                    "sine"
0.0 cos = 1.0                                               "cosine"
0.0 tan = 0.0                                               "tangent"
(10 max: 20) = 20                                           "get maximum of two numbers"
(10 min: 20) = 10                                           "get minimum of two numbers"
Float pi = 3.141592653589793                                "pi"
Float e = 2.718281828459045                                 "exp constant"
100 atRandom                                                "random number in (1,self)"

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

'Bitwise Manipulation'

(16rFF bitAnd: 16r0F) = 16r0F                               "and bits"
(16rF0 bitOr: 16r0F) = 16rFF                                "or bits"
(16rFF bitXor: 16r0F) = 16rF0                               "xor bits"
16rFF bitInvert = -16r100                                   "invert bits"
(16r0F bitShift: 4) = 16rF0                                 "left shift"
(16rF0 bitShift: -4) = 16r0F                                "right shift"
(16r80 bitAt: 7) = 0                                        "bit at position (0|1) [!Squeak]"
(16rFF allMask: 16r0F) = true                               "test if all bits set in mask set in receiver"
(16rFF anyMask: 16r0F) = true                               "test if any bits set in mask set in receiver"
(16rFF noMask: 16r0F) = false                               "test if all bits set in mask clear in receiver"

'Booleans'

123 isKindOf: Number                                        "test if object is class or subclass of"
123 isMemberOf: SmallInteger                                "test if object is type of class"
123 respondsTo: #sqrt                                       "test if object responds to message"
true = true                                                 "constant"
false = false                                               "constant"
1 = 1 = true                                                "equality predicate (operator)"
1 ~= 2 = true                                               "equality predicate (operator)"
(true and: false) = false                                   "logical and"
(true or: false) = true                                     "logical or"
true & false = false                                        "logical and (operator)"
true | false = true                                         "logical or (operator)"
(false and: ['and' error]) = false                          "logical and (short-circuit)"
(true or: ['or' error]) = true                              "logical or"
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

'Collection'

#(1 3 2 9 4 5 7 8 6) max = 9                                "maximum item"

'Conversion'

3.99 asInteger = 4                                          "convert number to integer (truncates in Squeak)"
3.99 asFraction                                             "convert number to fraction (399/100)"
3 asFloat                                                   "convert number to float"
65 asCharacter = $A                                         "convert integer to character"
$A asciiValue = 65                                          "convert character to integer"
3.99 printString = '3.99'                                   "convert object to string via printOn:"
3.99 storeString = '3.99'                                   "convert object to string via storeOn:"
(15 radix: 16) = '16rF'                                     "convert to string in given base"

'Dictionary ; Smalltalk'

(Smalltalk at: #SinOsc) = SinOsc                            "lookup value in Smalltalk dictionary"
(Smalltalk at: #DoesNotExist ifAbsent: [ nil ]) = nil       "lookup value with clause for absent values "
(Smalltalk includesKey: #DoesNotExist) = false              "see if key exists in dictionary"
Smalltalk keys size                                         "get set of all known keys "

'Symbols'

(#a , #b) = 'ab'                                            "Symbol -> Symbol -> String"

'Debugging'

nil yourself = nil                                          "returns receiver"
nil inspect                                                 "open object inspector window"
nil halt                                                    "breakpoint to halt interpreter"
nil halt: 'Reason'                                          "halt with indicateed text"
nil error: 'Reason'                                         "error with indicated text"
nil shouldNotImplement                                      "flag message should not be implemented"
nil subclassResponsibility                                  "flag message as abstract"
nil primitiveFailed                                         "system primitive failed"
