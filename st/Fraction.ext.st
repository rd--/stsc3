!Fraction methodsFor: 'printing'!
printHaskellOn: fd
    "Print my hsc3 representation."
    (self asFloat asString) putOn: fd!
!

