!Dictionary class methodsFor: 'instance creation'!
newFromAssociations: aCollection
    "Generate a dictionary from a collection of associations."
    | answer |
    answer := Dictionary new.
    aCollection do: [ :anAssociation | answer add: anAssociation ].
    ^answer!
!
