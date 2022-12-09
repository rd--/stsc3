# Protocols

## Object

Meta:

- _class_ - Answer the object which is the receiver's class.
- _respondsTo: aSymbol_ - Answer whether the method dictionary of the receiver's class or one of its superclasses contains the argument, aSymbol, as a message selector

Comparing:

- _== anObject_ - Answer whether the receiver and the argument are the same object.
- _= anObject_ - Answer whether the receiver and the argument represent the same component.
- _~~ anObject_ - Answer whether the receiver and the argument are not the same object.
- _~= anObject_ - Answer whether the receiver and the argument do not represent the same component.

Testing:

- _isNil_ - Answer whether the receiver is nil.
- _notNil_ - Answer whether the receiver is not nil

Copying:

- _copy_ - Answer another instance just like the receiver.
- _shallowCopy_ - Answer a copy of the receiver which shares the receiver's instance variables.
- _deepCopy_ - Answer a copy of the receiver with its own copy of each instance variable.

Accessing:

- _at: index_ - Answer the value of the indexed instance variable of the receiver whose index is the argument, index. If the receiver does not have indexed variables, or if the argument is greater than the number of indexed variables, then report an error.
- _at: index put: anObject_ - Store the argument, anObject, as the value of the indexed instance variable of the receiver whose index is the argument, index. If the receiver does not have indexed variables, or if the argument is greater than the number of indexed variables,
- _size_ - Answer the receiver's number of indexed variables. This value is the same as the largest legal index.

Printing:

- _printString_ - Answer a String whose characters are a description of the receiver.
- _printOn: aStream_ - Append to the argument, aStrearn, a String whose characters are a description of the receiver.

Storing:

- _storeString_ - Answer a String representation of the receiver from which the receiver can be reconstructed.
- _storeOn: aStream_ - Append to the argument, aStrearn, a String representation of the receiver from which the receiver can be reconstructed.

Error handling:

- _doesNotUnderstand: aMessage_ - Report to the user that the receiver does not understand the argument, aMessage, as a message.
- _error: aString_ - Report to the user that an error occurred in the context of responding to a message to the receiver. The report uses the argument, aString, as part of the error notification comment.
- _primitiveFailed_ - Report to the user that a method implemented as a system primitive has failed.
- _shouldNotlmplement_ - Report to the user that, although the superclass of the receiver specifies that a message should be implemented by subclasses, the class of the receiver cannot provide an appropriate implementation.
- _subclassResponsibility_ - Report to the user that a method specified in the superclass of the receiver should have been implemented in the receiver's class.

## Magnitude

Comparing:

- _< aMagnitude_ - Answer whether the receiver is less than the argument.
- _<= aMagnitude_ - Answer whether the receiver is less than or equal to the argument.
- _> aMagnitude_ - Answer whether the receiver is greater than the argument.
- _>= aMagnitude_ - Answer whether the receiver is greater than or equal to the argument.
- _between: min and: max_ - Answer whether the receiver is greater than or equal to the argument, rain, and less than or equal to the argument, max

Testing:

- _min: aMagnitude_ - Answer the receiver or the argument, whichever has the lesser magnitude.
- _max: aMagnitude_ - Answer the receiver or the argument, whichever has the greater magnitude.

## Number

Arithmetic:

- _+ aNumber_ - Answer the sum of the receiver and the argument, aNumber.
- _- aNumber_ - Answer the difference between the receiver and the argument, aNumber.
- _* aNumber_ - Answer the result of multiplying the receiver by the argument, aNumber.
- _/ aNumber_ - Answer the result of dividing the receiver by the argument, aNumber. Note that since as much precision as possible is retained, if the division is not exact, the result will be an instance of Fraction.
- _// aNumber_ - Answer the integer quotient defined by division with truncation toward negative infinity.
- _\\ aNumber_ - Answer the integer remainder defined by division with truncation toward negative infinity. This is the modulo operation.
- _abs_ - Answer a Number that is the absolute value (positive magnitude) of the receiver.
- _negated_ - Answer a Number that is the negation of the receiver.
- _quo: aNumber_ - Answer the integer quotient defined by division with truncation toward zero.
- _rem: aNumber_ - Answer the integer remainder defined by division with truncation toward zero.
- _reciprocal_ - Answer 1 divided by the receiver. Report an error to the user if the receiver is 0.

# Collection

Adding:

- _add: newObject_ - Include the argument, newObject, as one of the receiver's elements. Answer newObject.
- _addAll: aCollection_ - Include all the elements of the argument, aCollection, as the receiver's elements. Answer aCollection.

Removing:

- _remove: oldObject_ - Remove the argument, oldObject, from the receiver's elements. Answer oldObject unless no element is equal to oldObject, in which case, report that an error occurred.
- _remove: oldObject ifAbsent: anExceptionBIock_ - Remove the argument, oldObject, from the receiver's elements. If several of the elements are equal to oldObject, only one is removed. If no element is equal to oldObject, answer the result of evaluating anExceptionBIock. Otherwise, answer oldObject.
- _removeAll: aCollection_ - Remove each element of the argument, aCollection, from the receiver. If successful for each, answer aCollection. Otherwise report that an error occurred.

Testing:

- _includes: anObject_ - Answer whether the argument, anObject, is equal to one of the receiver's elements.
- _isEmpty_ - Answer whether the receiver contains any elements.
- _occurrencesOf: anObject_ - Answer how many of the receiver's elements are equal to the argument, anObject
