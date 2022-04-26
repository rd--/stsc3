Ugen subclass: #UnaryOpUgen
  instanceVariableNames: 'a'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

UnaryOpUgen comment: 'Unary Operators'!

!UnaryOpUgen methodsFor: 'accessing'!
  a ^a!
  a: x a := x!
!

!UnaryOpUgen class methodsFor: 'instance creation'!
  specialIndex: p1 a: p2
    "Create unary operator Ugen"
    | u |
    u := self new.
    u ugenName: #UnaryOpUGen. "note case"
    u paramNames: #(#a).
    u mceNames: #().
    u ugenRate: #(0).
    u numChan: 1.
    u specialIndex: p1.
    u ugenId: nil.
    u a: p2.
    ^u
  !
!
