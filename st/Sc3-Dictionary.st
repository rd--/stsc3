!Dictionary class methodsFor: 'instance creation'!
newFromAssociations: aCollection
    "Generate a dictionary from a collection of associations."
    | aDictionary |
    aDictionary := Dictionary new.
    aCollection do: [ :anAssociation | aDictionary add: anAssociation ].
    ^aDictionary! !
