!Array class methodsFor: 'enumerating'!
series: size value: start value: step
    "Generate an arithmetic series (ramp) of size places from start by step.  From Sc."
    ^(1 to: size) collect: [ :item | (step * (item - 1)) + start ]
!
geom: size value: start value: grow
    "Generate a geometric series (curve) of size places from start by grow.  From Sc."
    | accum |
    accum := start.
    ^(1 to: size) collect: [ :unusedItem |
      | entry |
      entry := accum.
      accum := grow * accum.
      entry ]
!
fill: size value: aBlock
    "Generate an array using a zero argument block to fill each slot.  From Sc.
     The argument as index form is not implemented since St and Sc disagree about indexing."
    ^(1 to: size) collect: [ :unusedItem | aBlock value ]
!
!

!Array methodsFor: 'randomness'!
choose
    "Choose a random element of myself.  Alias for atRandom."
    ^self atRandom
!
!

!Array methodsFor: 'rate control'!
kr
    ^self asUgen kr
!
!

!Array methodsFor: 'collecting'!
  addToUgenSet: c
    self do: [:x | x addToUgenSet: c]
  !
!

!Array methodsFor: 'multi-channel expansion'!
  asUgen
    "Array as Ugen (ie. Mce)"
    ^Mce source: self
  !
!
