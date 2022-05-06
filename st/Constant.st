AbstractUgen subclass: #Constant
  instanceVariableNames: 'value'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Constant comment: 'Constant numerical value. Required for adaptToAndSend messages.'!

!Constant methodsFor: 'initialization'!
value: aNumber
    aNumber isNumber ifTrue: [ value := aNumber ] ifFalse: [self error: 'not a number?']!
!

!Constant methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPutAll: '(constant (';
      nextPutAll: value asString;
      nextPutAll: ')) '!
!

!Constant methodsFor: 'testing'!
isZero
    ^value isZero!
!

!Constant class methodsFor: 'instance creation'!
value: aNumber
    ^self new value: aNumber!
!

!Constant methodsFor: 'rate'!
rate
    ^Rate ir!
!
