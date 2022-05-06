Object subclass: #UgenBuilder
  instanceVariableNames: ''
  classVariableNames: ''
  poolDictionaries: ''
  category: 'Sound-Sc3'!

UgenBuilder comment: 'Construct Ugens performing channel expansion'!

!UgenBuilder class methodsFor: 'instance creation'!
name: name numChan: numChan rateSpec: rateSpec specialIndex: specialIndex inputArray: inputArray
    inputArray containsArray ifTrue: [
        ^inputArray mceTransform collect: [ :item |
            UgenBuilder name: name numChan: numChan rateSpec: rateSpec specialIndex: specialIndex inputArray: item
        ]
    ] ifFalse: [
        | rate ugen |
        rate := rateSpec isArray
            ifTrue: [ ((inputArray atAll: rateSpec) collect: [ :item | item rate ]) max ]
            ifFalse: [ rateSpec ].
        ugen := PrimitiveUgen name: name numChan: numChan rate: rate specialIndex: specialIndex inputArray: inputArray.
        (numChan = 0) ifTrue: [ ^PrimitiveUgenOutput ugen: ugen index: -1. ].
        (numChan = 1) ifTrue: [ ^PrimitiveUgenOutput ugen: ugen index: 1. ]. "one-indexed"
        ^(1 to: numChan) collect: [ :item | PrimitiveUgenOutput ugen: ugen index: item ]
    ]!
!
