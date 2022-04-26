!Dictionary class methodsFor: 'instance creation'!
newFromAssociations: assocArray
    "Generate a dictionary from an Array of associations."
    | aDict |
    aDict := Dictionary new.
    assocArray do: [ :anAssoc | aDict add: anAssoc ].
    ^aDict
!
!
