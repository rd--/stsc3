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
        | rate scUgen |
        rate := rateSpec isArray
            ifTrue: [ ((inputArray atAll: rateSpec) collect: [ :item | item rate ]) max ]
            ifFalse: [ rateSpec ].
        scUgen := ScUgen name: name numChan: numChan rate: rate specialIndex: specialIndex inputArray: inputArray.
        (numChan = 0) ifTrue: [ ^Ugen scUgen: scUgen port: nil ].
        (numChan = 1) ifTrue: [ ^Ugen scUgen: scUgen port: 1 ].
        ^(1 to: numChan) collect: [ :item | Ugen scUgen: scUgen port: item ] "one-indexed"
    ]!
!
