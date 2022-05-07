!Boolean methodsFor: '.stc'!
if: trueBlock value: falseBlock
    "Alias for ifTrue:ifFalse:.  This is the SuperCollider if method, for .stc notation.
     To be removed, use p.ifTrue(q, ifFalse: r)."
    ^self ifTrue: trueBlock ifFalse: falseBlock!
!

!Boolean methodsFor: 'collecting'!
addToUgenSet: c protectedBy: w
    ^nil!
!
