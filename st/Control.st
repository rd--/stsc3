AbstractUgen subclass: #Control
  instanceVariableNames: 'name init'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Control comment: 'Control Input'!

!Control methodsFor: 'accessing'!
name
    ^name
!
name: x
    name := x
!
init
    ^init
!
init: x
    init := x!
!

!Control methodsFor: 'initialising'!
name: p1 init: p2
    name := p1.
    init := p2!
!

!Control methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    fd
      nextPutAll: '(control kr ';
      nextPutAll: self name asString asHaskellString;
      space;
      nextPutAll: self init asString;
      nextPutAll: ') '
  !
!

!Control class methodsFor: 'instance creation'!
name: p1 init: p2
    ^self new name: p1 init: p2!
!
