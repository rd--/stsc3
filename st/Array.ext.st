!Array methodsFor: 'mce'!
containsCollection
    self do: [ :item | item isCollection ifTrue: [ ^true ] ].
    ^false!
mceTransform
    ^self extendToBeOfEqualSize transpose!
asArray
    ^self!
!

!Array methodsFor: 'supercollider'!
perform: aSelector crossedWith: anArray
    "Perform aSelector for each of my items with each of anArrays items in turn (p *.x q in Sc)"
    | answerArray nextIndex |
    answerArray := Array new: self size * anArray size.
    nextIndex := 1.
    self do: [ :leftItem |
        anArray do: [ :rightItem |
            answerArray at: nextIndex put: (leftItem perform: aSelector with: rightItem).
            nextIndex := nextIndex + 1
        ]
    ].
    ^answerArray!
allTuples
    "Answer all of the possible n-tuples of each of my elements (I am an n-element array of any sized arrays)."
    | answerSize |
    answerSize := (self collect: [ :item | item size ]) product.
    ^(1 to: answerSize) collect: [ :i |
        | k nextTuple |
        k := i - 1.
        nextTuple := Array new: self size.
        (self size to: 1 by: -1) collect: [ :j |
            | fromArray |
            fromArray := self at: j.
            nextTuple at: j put: (fromArray at: (k \\ fromArray size) + 1).
            k := k // fromArray size
        ].
        nextTuple
    ]!
!

!Array methodsFor: 'panning'!
splay2
    ^Splay2 inArray: self!
!

!Array methodsFor: 'klang data'!
asKlangSpec
    ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
asKlankSpec
    ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
!

!Array methodsFor: 'unit generator graph'!
play
    Sc3 play: self!
draw
    Sc3 draw: self!
printUgens
    Sc3 printUgens: self!
!
