AbstractUgen subclass: #Control
  instanceVariableNames: 'name init'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'
!

Control comment: 'Control Input'!

!Control methodsFor: 'collecting'!
  addToUgenSet: c
  !
!

!Control methodsFor: 'accessing'!
  name ^name!
  name: x name := x!
  init ^init!
  init: x init := x!
!

!Control class methodsFor: 'instance creation'!
  name: p1 init: p2
    | u |
    u := self new.
    u name: p1.
    u init: p2.
    ^u
  !
!
