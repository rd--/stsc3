!Array class methodsFor: 'enumerating'!
series: size value: start value: step
    "Generate an arithmetic series (ramp) of size places from start by step.  From Sc."
    ^(1 to: size) collect: [ :item | (step * (item - 1)) + start ]!
geom: size value: start value: grow
    "Generate a geometric series (curve) of size places from start by grow.  From Sc."
    | accum |
    accum := start.
    ^(1 to: size) collect: [ :unusedItem |
      | entry |
      entry := accum.
      accum := grow * accum.
      entry ]!
fill: size value: aBlock
    "Generate an array using a zero argument block to fill each slot.  From Sc.
     The argument as index form is not implemented since St and Sc disagree about indexing."
    ^(1 to: size) collect: [ :unusedItem | aBlock value ]!
!

!Array methodsFor: 'randomness'!
choose
    "Choose a random element of myself.  Alias for atRandom."
    ^self atRandom!
!

!Array methodsFor: 'rate control'!
kr
    ^self asUgen kr!
!

!Array methodsFor: 'collecting'!
addToUgenSet: c
    self do: [:x | x addToUgenSet: c]!
!

!Array methodsFor: 'multi-channel expansion'!
asUgen
    "Array as Ugen (ie. Mce)"
    ^Mce source: self!
!

!Array methodsFor: 'arithmetic'!
perform: aSelector crossedWith: anArray
    "Perform aSelector for each of my items with each of anArrays items in turn.  This is p *.x q in Sc.
    | x | x := #(#(4 4.5) #(2 3 5 6)). x perform: #* crossedWith: x
    "
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
!

!Array methodsFor: 'combinatorics'!
allTuples
    "Answer all of the possible n-tuples of each of my elements.  I am an n-element array of any sized arrays.  From Sc.
    #(#(1 2) #(3 4)) allTuples
    #(#(1 2) #(3 4) #(4 5) #(6 7 8 9)) allTuples
    "
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

!Array methodsFor: 'Klang data'!
asKlangSpec
    ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
asKlankSpec
    ^KlankSpec freq: (self at: 1) amp: (self at: 2) time: (self at: 3)!
!

!Array methodsFor: 'printing'!
printHaskellArrayOn: fd
    "Print my hsc3 representation."
    '[' putOn: fd.
    self do: [ :x | x printHaskellOn: fd. ] separatedBy: [ ',' putOn: fd ].
    ']' putOn: fd
  !
printHaskellOn: fd
    "Print my hsc3 Mce representation."
    '(mce ' putOn: fd.
    self printHaskellArrayOn: fd.
    ')' putOn: fd
  !
writeHaskellTo: fn
    "Allow Array to be root of Ugen graph"
    self asUgen writeHaskellTo: fn
  !
play
    "Allow Array to be root of Ugen graph"
    self asUgen play.
  !
draw
    "Allow Array to be root of Ugen graph"
    self asUgen draw.
  !
!
