!Dictionary class methodsFor: 'stsc3'!
newFrom: aCollection
    | answer |
    answer := Dictionary new.
    aCollection do: [ :anAssociation | answer add: anAssociation ].
    ^answer!
newFromPairs: anArray
    | newDictionary |
    newDictionary := self new: (anArray size / 2).
    1 to: (anArray size - 1) by: 2 do: [ :item |
        newDictionary at: (anArray at: item) put: (anArray at: item + 1)
    ].
    ^newDictionary!
!
