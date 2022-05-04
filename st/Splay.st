AbstractUgen subclass: #Splay
  instanceVariableNames: 'paramValues'
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

Splay comment: 'Pan a set of channels across the stereo field.'!
!Splay methodsFor: 'accessing'!
paramValues
    ^paramValues!
paramValues: anArray
    ^paramValues := anArray!
!

!Splay methodsFor: 'collecting'!
addToUgenSet: aSet
    paramValues do: [ :item | item addToUgenSet: aSet ]!
!

!Splay methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    '(splay ' putOn: fd.
    paramValues do: [ :item | item printHaskellOn: fd ] separatedBy: [ ' ' putOn: fd ].
    ') ' putOn: fd
  !
!


!Splay class methodsFor: 'instance creation'!
input: input spread: spread level: level center: center levelComp: levelComp
    | u |
    u := self new.
    u paramValues: {input. spread. level. center. levelComp}.
    ^u!
primaryFactoryMethod
    ^#input:spread:level:center:levelComp:!
input: input spread: spread level: level center: center
    ^Splay input: input spread: spread level: level center: center levelComp: true!
input: input level: level
    ^Splay input: input spread: 1 level: level center: 0 levelComp: true!
input: input
    ^Splay input: input spread: 1 level: 1 center: 0 levelComp: true!
!



